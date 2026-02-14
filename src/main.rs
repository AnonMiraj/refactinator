mod bazel;
mod cmake;
mod cpp;

use anyhow::Result;
use clap::Parser;
use regex::Regex;
use std::fs;
use std::path::PathBuf;

use crate::cpp::CppRefactorer;
use crate::cpp::Edit;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    func: String,

    #[arg(short, long, default_value = ".")]
    root: PathBuf,

    #[arg(long = "no-constexpr", short = 'n', alias = "no")]
    no_constexpr: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let func = &args.func;
    let root = &args.root;

    let generic_cpp_path = root.join(format!("libc/src/math/generic/{}.cpp", func));
    let mut use_f16 = false;
    let mut use_f128 = false;
    let mut extracted_args = String::new();
    let mut extracted_ret_type = String::new();

    if generic_cpp_path.exists() {
        let content = fs::read_to_string(&generic_cpp_path).unwrap_or_default();
        use_f128 = content.contains("float128") || content.contains("f128");

        let content_no_bf16 = content.replace("bfloat16", "").replace("bf16", "");
        use_f16 = content_no_bf16.contains("float16") || content_no_bf16.contains("f16");

        let re = Regex::new(&format!(
            r"LLVM_LIBC_FUNCTION\s*\(\s*([^,]+),\s*{},\s*\(([^)]*)\)\s*\)",
            regex::escape(func)
        ))
        .unwrap();
        if let Some(caps) = re.captures(&content) {
            extracted_ret_type = caps[1].trim().to_string();
            extracted_args = caps[2].trim().to_string();
        }
    }

    println!(
        "Refactoring function: {} (f16: {}, f128: {})",
        func, use_f16, use_f128
    );

    let shared_math_func_path = root.join(format!("libc/shared/math/{}.h", func));
    if !shared_math_func_path.exists() {
        let content = cpp::generate_shared_wrapper(func, use_f16, use_f128);
        fs::write(&shared_math_func_path, content)?;
        println!("Created: {:?}", shared_math_func_path);
    }

    let shared_math_path = root.join("libc/shared/math.h");
    if shared_math_path.exists() {
        let content = fs::read_to_string(&shared_math_path)?;

        let include_line = format!("#include \"math/{}.h\"", func);

        if !content.contains(&format!("math/{}.h", func)) {
            let mut refactor = CppRefactorer::new(content.clone(), args.no_constexpr)?;
            let re = Regex::new(r"(?m)^#include.*$").unwrap();
            if let Some(mat) = re.find_iter(&content).last() {
                let pos = mat.end();
                refactor.edits.push(Edit {
                    range: pos..pos,
                    new_text: format!("\n{}", include_line),
                });
                fs::write(&shared_math_path, refactor.apply())?;
                println!("Updated: {:?}", shared_math_path);
            }
        }
    }

    let support_path = root.join(format!("libc/src/__support/math/{}.h", func));

    if !support_path.exists() {
        if generic_cpp_path.exists() {
            cpp::extract_and_create_support_header(
                func,
                &generic_cpp_path,
                &support_path,
                use_f16,
                use_f128,
                args.no_constexpr,
            )?;
        } else {
            println!("Warning: Support file missing and generic CPP missing.");
        }
    } else {
        let content = fs::read_to_string(&support_path)?;
        let mut refactor = CppRefactorer::new(content, args.no_constexpr)?;
        refactor.transform_implementation_header();
        fs::write(&support_path, refactor.apply())?;
        println!("Refactored Implementation: {:?}", support_path);
    }

    if generic_cpp_path.exists() {
        cpp::regenerate_generic_cpp(func, &generic_cpp_path)?;
    }

    cmake::update_cmake_files(root, func, use_f16, use_f128)?;
    bazel::update_bazel_files(root, func, use_f16, use_f128)?;
    cpp::update_shared_test_file(root, func, &extracted_args, &extracted_ret_type)?;
    Ok(())
}
