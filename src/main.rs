use anyhow::{Context, Result};
use clap::Parser;
use regex::{Regex, RegexBuilder};
use std::cmp::Reverse;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};
use tree_sitter::{Node, Parser as TSParser};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    func: String,

    #[arg(short, long, default_value = ".")]
    root: PathBuf,
}

#[derive(Debug, Clone)]
struct Edit {
    range: Range<usize>,
    new_text: String,
}

struct CppRefactorer {
    source: String,
    edits: Vec<Edit>,
    parser: TSParser,
}

impl CppRefactorer {
    fn new(source: String) -> Result<Self> {
        let mut parser = TSParser::new();
        parser.set_language(tree_sitter_cpp::language())?;
        Ok(Self {
            source,
            edits: Vec::new(),
            parser,
        })
    }

    fn apply(mut self) -> String {
        self.edits.sort_by_key(|e| Reverse(e.range.start));
        let mut result = self.source.clone();
        for edit in self.edits {
            if edit.range.end <= result.len() {
                result.replace_range(edit.range, &edit.new_text);
            }
        }
        result
    }

    fn transform_implementation_header(&mut self) {
        let tree = self.parser.parse(&self.source, None).unwrap();
        let root = tree.root_node();

        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            if child.kind() == "namespace_definition" {
                self.inject_namespace_math(child);
            }
        }

        self.recurse_transform_funcs(root);
    }

    fn recurse_transform_funcs(&mut self, node: Node) {
        let kind = node.kind();
        if kind == "expression_statement" || kind == "function_definition" {
            let text = &self.source[node.byte_range()];
            if text.trim_start().starts_with("LLVM_LIBC_FUNCTION") {
                self.convert_macro_to_static(node);
                return;
            }
        }

        if kind == "function_definition" {
            let text = &self.source[node.byte_range()];
            if !text.contains("LLVM_LIBC_FUNCTION") {
                self.add_modifiers(node);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.recurse_transform_funcs(child);
        }
    }

    fn convert_macro_to_static(&mut self, node: Node) {
        let text = &self.source[node.byte_range()];
        let re = Regex::new(
            r"LLVM_LIBC_FUNCTION\s*\(\s*([^,]+),\s*([^,]+),\s*\(([^)]*)\)\s*\)\s*(\{[\s\S]*)",
        )
        .unwrap();

        if let Some(caps) = re.captures(text) {
            let ret_type = caps[1].trim();
            let name = caps[2].trim();
            let args = caps[3].trim();
            let body_start_idx = text.find('{').unwrap_or(text.len());
            let body = &text[body_start_idx..];

            let new_sig = format!(
                "LIBC_INLINE static constexpr {} {}({}) {}",
                ret_type, name, args, body
            );

            self.edits.push(Edit {
                range: node.byte_range(),
                new_text: new_sig,
            });
        }
    }

    fn add_modifiers(&mut self, node: Node) {
        let body = node.child_by_field_name("body");
        let end_of_sig = body.map(|b| b.start_byte()).unwrap_or(node.end_byte());
        let sig_range = node.start_byte()..end_of_sig;
        let sig_text = &self.source[sig_range.clone()];

        if !sig_text.contains("static") && !sig_text.contains("constexpr") {
            self.edits.push(Edit {
                range: node.start_byte()..node.start_byte(),
                new_text: "LIBC_INLINE static constexpr ".to_string(),
            });
        }
    }

    fn inject_namespace_math(&mut self, namespace_node: Node) {
        if let Some(body) = namespace_node.child_by_field_name("body") {
            let start = body.start_byte() + 1;
            self.edits.push(Edit {
                range: start..start,
                new_text: "\n\nnamespace math {\n".to_string(),
            });

            let end = body.end_byte() - 1;
            self.edits.push(Edit {
                range: end..end,
                new_text: "\n} // namespace math\n".to_string(),
            });
        }
    }

    fn update_entrypoint(&mut self, func_name: &str) {
        let tree = self.parser.parse(&self.source, None).unwrap();
        let root = tree.root_node();
        let mut last_include_pos = 0;
        let mut cursor = root.walk();

        let entry_header = format!("src/math/{}.h", func_name);
        let support_header = format!("src/__support/math/{}.h", func_name);

        for child in root.children(&mut cursor) {
            if child.kind() == "preproc_include" {
                let text = &self.source[child.byte_range()];
                last_include_pos = child.end_byte();

                if !text.contains(&entry_header) && !text.contains(&support_header) {
                    let start = child.start_byte();
                    let mut end = child.end_byte();
                    if end < self.source.len() && &self.source[end..end + 1] == "\n" {
                        end += 1;
                    }
                    self.edits.push(Edit {
                        range: start..end,
                        new_text: "".to_string(),
                    });
                }
            }
        }

        if !self.source.contains(&support_header) {
            self.edits.push(Edit {
                range: last_include_pos..last_include_pos,
                new_text: format!("\n#include \"{}\"", support_header),
            });
        }

        self.replace_entrypoint_body(root, func_name);
    }

    fn replace_entrypoint_body(&mut self, node: Node, func_name: &str) {
        let kind = node.kind();
        if kind == "function_definition" || kind == "expression_statement" {
            let text = &self.source[node.byte_range()];
            if text.contains("LLVM_LIBC_FUNCTION") && text.contains(func_name) {
                let re =
                    Regex::new(r"LLVM_LIBC_FUNCTION\s*\(\s*[^,]+,\s*[^,]+,\s*\(([^)]*)\)\s*\)")
                        .unwrap();

                if let Some(caps) = re.captures(text) {
                    let args_block = caps[1].trim();
                    let arg_names: Vec<&str> = args_block
                        .split(',')
                        .map(|arg| arg.trim().split_whitespace().last().unwrap_or(""))
                        .filter(|s| !s.is_empty())
                        .collect();

                    let call_args = arg_names.join(", ");

                    if let Some(body_match) = text.find('{') {
                        let body_start = node.start_byte() + body_match;
                        let body_end = node.end_byte();
                        let new_body =
                            format!("{{\n  return math::{}({});\n}}", func_name, call_args);

                        self.edits.push(Edit {
                            range: body_start..body_end,
                            new_text: new_body,
                        });
                    }
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.replace_entrypoint_body(child, func_name);
        }
    }
}

fn extract_and_create_support_header(
    func_name: &str,
    cpp_path: &Path,
    header_path: &Path,
    use_f16: bool,
    use_f128: bool,
) -> Result<()> {
    let cpp_content = fs::read_to_string(cpp_path).context("Could not read generic CPP file")?;
    let mut extra_includes = String::new();

    for line in cpp_content.lines() {
        if line.starts_with("#include") {
            if line.contains(&format!("src/math/{}.h", func_name)) {
                continue;
            }
            if line.contains("float16-macros.h") || line.contains("float128-macros.h") {
                continue;
            }
            extra_includes.push_str(line);
            extra_includes.push('\n');
        }
    }

    let re = Regex::new(&format!(
        r"LLVM_LIBC_FUNCTION\s*\(\s*([^,]+),\s*{},\s*\(([^)]*)\)\s*\)\s*(\{{[\s\S]*?\n\}})",
        regex::escape(func_name)
    ))
    .unwrap();

    if let Some(caps) = re.captures(&cpp_content) {
        let ret_type = caps[1].trim();
        let args = caps[2].trim();
        let body = caps[3].trim();

        let (macro_include, pre_guard, post_guard) = if use_f128 {
            (
                "#include \"include/llvm-libc-macros/float128-macros.h\"\n",
                "\n#ifdef LIBC_TYPES_HAS_FLOAT128\n",
                "\n#endif // LIBC_TYPES_HAS_FLOAT128\n",
            )
        } else if use_f16 {
            (
                "#include \"include/llvm-libc-macros/float16-macros.h\"\n",
                "\n#ifdef LIBC_TYPES_HAS_FLOAT16\n",
                "\n#endif // LIBC_TYPES_HAS_FLOAT16\n",
            )
        } else {
            ("", "", "")
        };

        let title = format!("Implementation header for {}", func_name);
        let dashes_count = (80usize.saturating_sub(8 + title.len() + 17)).max(0);
        let first_line = format!(
            "//===-- {} {}-*- C++ -*-===//",
            title,
            "-".repeat(dashes_count)
        );

        let header_content = format!(
            r#"{first_line}
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIBC_SRC___SUPPORT_MATH_{func_upper}_H
#define LLVM_LIBC_SRC___SUPPORT_MATH_{func_upper}_H

{macro_include}{pre_guard}
{extra_includes}

namespace LIBC_NAMESPACE_DECL {{

namespace math {{

LIBC_INLINE static constexpr {ret} {func}({args}) {body}

}} // namespace math

}} // namespace LIBC_NAMESPACE_DECL
{post_guard}
#endif // LLVM_LIBC_SRC___SUPPORT_MATH_{func_upper}_H
"#,
            first_line = first_line,
            func = func_name,
            func_upper = func_name.to_uppercase(),
            ret = ret_type,
            args = args,
            body = body,
            macro_include = macro_include,
            pre_guard = pre_guard,
            post_guard = post_guard,
            extra_includes = extra_includes
        );

        fs::write(header_path, header_content)?;
        println!("Extracted implementation to: {:?}", header_path);
    } else {
        println!(
            "Could not find LLVM_LIBC_FUNCTION for {} in CPP file.",
            func_name
        );
    }
    Ok(())
}

fn update_cmake_files(root: &Path, func: &str) -> Result<()> {
    println!("Updating CMakeLists.txt files...");
    let generic_cmake_path = root.join("libc/src/math/generic/CMakeLists.txt");
    let support_cmake_path = root.join("libc/src/__support/math/CMakeLists.txt");
    let header_path = root.join(format!("libc/src/__support/math/{}.h", func));

    let mut original_deps = Vec::new();
    if generic_cmake_path.exists() {
        if let Ok(deps) = extract_original_deps(&generic_cmake_path, func) {
            println!("  Extracted {} dependencies", deps.len());
            original_deps = deps;
        }
    }

    if support_cmake_path.exists() && header_path.exists() {
        update_support_cmake(&support_cmake_path, func, original_deps)?;
    }

    if generic_cmake_path.exists() {
        update_generic_cmake(&generic_cmake_path, func)?;
    }

    let test_cmake_path = root.join("libc/test/shared/CMakeLists.txt");
    if test_cmake_path.exists() {
        update_test_cmake(&test_cmake_path, func)?;
    }

    Ok(())
}

fn extract_original_deps(cmake_path: &Path, func: &str) -> Result<Vec<String>> {
    let content = fs::read_to_string(cmake_path)?;
    let pattern = format!(
        r"add_entrypoint_object\s*\(\s*{}\s*[\s\S]*?DEPENDS\s*([\s\S]*?)\)",
        regex::escape(func)
    );
    let re = RegexBuilder::new(&pattern)
        .multi_line(true)
        .build()
        .unwrap();

    let mut deps = Vec::new();
    if let Some(caps) = re.captures(&content) {
        let block = &caps[1];
        for token in block.split_whitespace() {
            let clean_token = token.trim();
            if !clean_token.is_empty() && clean_token != ")" {
                deps.push(clean_token.to_string());
            }
        }
    }
    Ok(deps)
}

fn update_support_cmake(cmake_path: &Path, func: &str, extra_deps: Vec<String>) -> Result<()> {
    let mut content = fs::read_to_string(cmake_path)?;

    if content.contains(&format!("add_header_library(\n  {}", func)) {
        println!("  Skipping support CMake (already exists)");
        return Ok(());
    }

    let mut deps = Vec::new();
    for original in extra_deps {
        if original.contains("libc.src.__support.") || original.contains("libc.hdr.") {
            deps.push(original);
        }
    }
    deps.sort();
    deps.dedup();

    let deps_str = deps.join("\n    ");
    let new_block = format!(
        r#"
add_header_library(
  {func}
  HDRS
    {func}.h
  DEPENDS
    {deps}
)
"#,
        func = func,
        deps = deps_str
    );

    content.push_str(&new_block);
    fs::write(cmake_path, content)?;
    println!("  Updated: {:?}", cmake_path);
    Ok(())
}

fn update_generic_cmake(cmake_path: &Path, func: &str) -> Result<()> {
    let content = fs::read_to_string(cmake_path)?;
    let pattern = format!(
        r"(add_entrypoint_object\s*\(\s*{}\s*[\s\S]*?DEPENDS)([\s\S]*?)(\))",
        regex::escape(func)
    );
    let re = RegexBuilder::new(&pattern)
        .multi_line(true)
        .build()
        .unwrap();

    if re.is_match(&content) {
        let replacement = format!(
            "$1\n    libc.src.__support.math.{}\n    libc.src.errno.errno\n$3",
            func
        );
        let new_content = re.replace(&content, replacement).to_string();
        fs::write(cmake_path, new_content)?;
        println!("  Updated: {:?}", cmake_path);
    }
    Ok(())
}

fn update_test_cmake(cmake_path: &Path, func: &str) -> Result<()> {
    let mut content = fs::read_to_string(cmake_path)?;
    let target_dep = format!("libc.src.__support.math.{}", func);

    if content.contains(&target_dep) {
        println!("  Skipping test CMake (already exists)");
        return Ok(());
    }

    let re = Regex::new(r"(libc\.src\.__support\.math\.[a-zA-Z0-9_]+)(\s*\n\s*\))").unwrap();
    let matches: Vec<_> = re.find_iter(&content).collect();

    if let Some(last_match) = matches.last() {
        let range = last_match.range();
        let matched_str = &content[range.clone()];
        if let Some(cap) = re.captures(matched_str) {
            let new_str = format!("{}\n    {}{}", &cap[1], target_dep, &cap[2]);
            content.replace_range(range, &new_str);
            fs::write(cmake_path, content)?;
            println!("  Updated: {:?}", cmake_path);
            return Ok(());
        }
    }

    println!("  Warning: Could not automatically insert into test CMake.");
    Ok(())
}

fn update_bazel_files(root: &Path, func: &str) -> Result<()> {
    println!("Updating Bazel BUILD file...");
    let bazel_path = root.join("utils/bazel/llvm-project-overlay/libc/BUILD.bazel");
    let support_cmake_path = root.join("libc/src/__support/math/CMakeLists.txt");

    if !bazel_path.exists() {
        println!("  Warning: Bazel file not found");
        return Ok(());
    }

    if support_cmake_path.exists() {
        update_bazel_support_target(&bazel_path, &support_cmake_path, func)?;
    } else {
        println!("  Warning: Support CMake not found, cannot extract deps for Bazel.");
    }

    update_bazel_math_function_target(&bazel_path, func)?;
    Ok(())
}

fn update_bazel_support_target(bazel_path: &Path, cmake_path: &Path, func: &str) -> Result<()> {
    let mut content = fs::read_to_string(bazel_path)?;
    let target_name = format!("__support_math_{}", func);

    if content.contains(&format!("name = \"{}\"", target_name)) {
        println!("  Skipping Bazel support target (already exists)");
        return Ok(());
    }

    let deps = get_bazel_deps_from_cmake(cmake_path, func)?;
    let deps_str = deps
        .iter()
        .map(|d| format!("\"{}\",", d))
        .collect::<Vec<_>>()
        .join("\n        ");

    let new_block = format!(
        r#"
libc_support_library(
    name = "{}",
    hdrs = ["src/__support/math/{}.h"],
    deps = [
        {}
    ],
)
"#,
        target_name, func, deps_str
    );

    content.push_str(&new_block);
    fs::write(bazel_path, content)?;
    println!("  Added support library to Bazel");
    Ok(())
}

fn update_bazel_math_function_target(bazel_path: &Path, func: &str) -> Result<()> {
    let content = fs::read_to_string(bazel_path)?;
    let pattern = format!(
        r#"(libc_math_function\s*\(\s*name\s*=\s*"{}"\s*,[\s\S]*?additional_deps\s*=\s*\[)([\s\S]*?)(\])"#,
        regex::escape(func)
    );
    let re = RegexBuilder::new(&pattern)
        .multi_line(true)
        .build()
        .unwrap();

    if re.is_match(&content) {
        let support_target = format!(":__support_math_{}", func);
        let replacement = format!(
            "$1\n        \"{}\",\n        \":errno\",\n    $3",
            support_target
        );
        let new_content = re.replace(&content, replacement).to_string();
        fs::write(bazel_path, new_content)?;
        println!("  Updated additional_deps for {} in Bazel", func);
    }
    Ok(())
}

fn get_bazel_deps_from_cmake(cmake_path: &Path, func: &str) -> Result<Vec<String>> {
    let content = fs::read_to_string(cmake_path)?;
    let mut deps = Vec::new();

    let pattern = format!(
        r"add_header_library\s*\(\s*{}\s*[\s\S]*?DEPENDS\s*([\s\S]*?)\)",
        regex::escape(func)
    );
    let re = RegexBuilder::new(&pattern).build().unwrap();

    if let Some(caps) = re.captures(&content) {
        let depends_block = &caps[1];
        for token in depends_block.split_whitespace() {
            let dep = token.trim();
            if dep.is_empty() || dep == ")" {
                continue;
            }

            if dep == "libc.src.errno.errno" {
                deps.push(":errno".to_string());
            } else if dep.starts_with("libc.src.__support.") {
                let parts: Vec<&str> = dep.split('.').collect();
                if parts.len() >= 5 {
                    let group = parts[3].to_lowercase();
                    let name = parts[4].to_lowercase();
                    deps.push(format!(":__support_{}_{}", group, name));
                }
            }
        }
    }

    deps.sort();
    deps.dedup();
    Ok(deps)
}

fn update_shared_test_file(root: &Path, func: &str, args_str: &str) -> Result<()> {
    let test_path = root.join("libc/test/shared/shared_math_test.cpp");
    if !test_path.exists() {
        println!(
            "Warning: Shared math test file not found at {:?}",
            test_path
        );
        return Ok(());
    }

    let mut content = fs::read_to_string(&test_path)?;
    if content.contains(&format!("shared::{}", func)) {
        println!("  Skipping shared test (already exists)");
        return Ok(());
    }

    // 1. Map function suffix to TEST block
    let (block_name, zero_lit, input_lit) = if func.ends_with("f16") {
        ("AllFloat16", "0x0p+0f16", "0.0f16")
    } else if func.ends_with("f128") {
        ("AllFloat128", "float128(0.0)", "float128(0.0)")
    } else if func.ends_with("f") {
        ("AllFloat", "0x0p+0f", "0.0f")
    } else if func.ends_with("l") {
        ("AllLongDouble", "0x0p+0L", "0.0L")
    } else {
        ("AllDouble", "0.0", "0.0")
    };

    // 2. Determine argument count from the passed string
    let mut arg_count = 1;
    if !args_str.trim().is_empty() {
        arg_count = args_str.split(',').count();
    }

    let inputs = vec![input_lit; arg_count].join(", ");
    let test_line = format!(
        "  EXPECT_FP_EQ({}, LIBC_NAMESPACE::shared::{}({}));",
        zero_lit, func, inputs
    );

    // 3. Inject into the correct TEST block
    let pattern = format!(
        r"(?s)TEST\(LlvmLibcSharedMathTest, {}\) \{{(.*?)\}}",
        block_name
    );
    let re = RegexBuilder::new(&pattern)
        .multi_line(true)
        .build()
        .unwrap();

    if let Some(caps) = re.captures(&content) {
        let full_match = caps.get(0).unwrap();
        let body = &caps[1];

        let mut new_body = body.to_string();
        if !new_body.trim_end().ends_with('\n') {
            new_body.push('\n');
        }
        new_body.push_str(&test_line);
        new_body.push('\n');

        let replacement = format!(
            "TEST(LlvmLibcSharedMathTest, {}) {{{}}}",
            block_name, new_body
        );
        content.replace_range(full_match.range(), &replacement);

        fs::write(&test_path, content)?;
        println!("  Updated shared test: {:?}", test_path);
    } else {
        println!(
            "  Warning: Could not find TEST block {} in shared_math_test.cpp",
            block_name
        );
    }

    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    let func = &args.func;
    let root = &args.root;

    let generic_cpp_path = root.join(format!("libc/src/math/generic/{}.cpp", func));
    let mut use_f16 = false;
    let mut use_f128 = false;
    let mut extracted_args = String::new();

    if generic_cpp_path.exists() {
        let content = fs::read_to_string(&generic_cpp_path).unwrap_or_default();
        use_f16 = content.contains("float16") || content.contains("f16");
        use_f128 = content.contains("float128") || content.contains("f128");

        // Extract args once here to use in multiple places
        let re = Regex::new(&format!(
            r"LLVM_LIBC_FUNCTION\s*\(\s*[^,]+,\s*{},\s*\(([^)]*)\)\s*\)",
            regex::escape(func)
        ))
        .unwrap();
        if let Some(caps) = re.captures(&content) {
            extracted_args = caps[1].trim().to_string();
        }
    }

    println!(
        "Refactoring function: {} (f16: {}, f128: {})",
        func, use_f16, use_f128
    );

    let shared_math_func_path = root.join(format!("libc/shared/math/{}.h", func));
    if !shared_math_func_path.exists() {
        let content = generate_shared_wrapper(func, use_f16, use_f128);
        fs::write(&shared_math_func_path, content)?;
        println!("Created: {:?}", shared_math_func_path);
    }

    let shared_math_path = root.join("libc/shared/math.h");
    if shared_math_path.exists() {
        let content = fs::read_to_string(&shared_math_path)?;

        let include_line = format!("#include \"math/{}.h\"", func);

        if !content.contains(&format!("math/{}.h", func)) {
            let mut refactor = CppRefactorer::new(content.clone())?;
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
            extract_and_create_support_header(
                func,
                &generic_cpp_path,
                &support_path,
                use_f16,
                use_f128,
            )?;
        } else {
            println!("Warning: Support file missing and generic CPP missing.");
        }
    } else {
        let content = fs::read_to_string(&support_path)?;
        let mut refactor = CppRefactorer::new(content)?;
        refactor.transform_implementation_header();
        fs::write(&support_path, refactor.apply())?;
        println!("Refactored Implementation: {:?}", support_path);
    }

    if generic_cpp_path.exists() {
        let content = fs::read_to_string(&generic_cpp_path)?;
        let mut refactor = CppRefactorer::new(content)?;
        refactor.update_entrypoint(func);
        fs::write(&generic_cpp_path, refactor.apply())?;
        println!("Updated Entrypoint: {:?}", generic_cpp_path);
    }

    update_cmake_files(root, func)?;
    update_bazel_files(root, func)?;
    update_shared_test_file(root, func, &extracted_args)?;
    Ok(())
}

fn generate_shared_wrapper(func: &str, use_f16: bool, use_f128: bool) -> String {
    let func_upper = func.to_uppercase();

    let (include_macro, pre_guard, post_guard) = if use_f128 {
        (
            "\n#include \"include/llvm-libc-types/float128.h\"",
            "\n#ifdef LIBC_TYPES_HAS_FLOAT128\n",
            "\n#endif // LIBC_TYPES_HAS_FLOAT128\n",
        )
    } else if use_f16 {
        (
            "\n#include \"include/llvm-libc-macros/float16-macros.h\"",
            "\n#ifdef LIBC_TYPES_HAS_FLOAT16\n",
            "\n#endif // LIBC_TYPES_HAS_FLOAT16\n",
        )
    } else {
        ("", "", "")
    };

    let title = format!("Shared {} function", func);
    let dashes_count = (80usize.saturating_sub(8 + title.len() + 17)).max(0);
    let first_line = format!(
        "//===-- {} {}-*- C++ -*-===//",
        title,
        "-".repeat(dashes_count)
    );

    format!(
        r#"{first_line}
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIBC_SHARED_MATH_{func_upper}_H
#define LLVM_LIBC_SHARED_MATH_{func_upper}_H
{include_macro}#include "shared/libc_common.h"{pre_guard}
#include "src/__support/math/{func}.h"

namespace LIBC_NAMESPACE_DECL {{
namespace shared {{

using math::{func};

}} // namespace shared
}} // namespace LIBC_NAMESPACE_DECL
{post_guard}
#endif // LLVM_LIBC_SHARED_MATH_{func_upper}_H
"#,
        first_line = first_line,
        func = func,
        func_upper = func_upper,
        include_macro = include_macro,
        pre_guard = pre_guard,
        post_guard = post_guard
    )
}
