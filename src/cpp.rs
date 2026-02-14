use anyhow::{Context, Result};
use regex::{Regex, RegexBuilder};
use std::cmp::Reverse;
use std::fs;
use std::ops::Range;
use std::path::Path;
use tree_sitter::{Node, Parser as TSParser};

#[derive(Debug, Clone)]
pub struct Edit {
    pub range: Range<usize>,
    pub new_text: String,
}

pub struct CppRefactorer {
    source: String,
    pub edits: Vec<Edit>,
    parser: TSParser,
}

impl CppRefactorer {
    pub fn new(source: String) -> Result<Self> {
        let mut parser = TSParser::new();
        parser.set_language(tree_sitter_cpp::language())?;
        Ok(Self {
            source,
            edits: Vec::new(),
            parser,
        })
    }

    pub fn apply(mut self) -> String {
        self.edits.sort_by_key(|e| Reverse(e.range.start));
        let mut result = self.source.clone();
        for edit in self.edits {
            if edit.range.end <= result.len() {
                result.replace_range(edit.range, &edit.new_text);
            }
        }
        result
    }

    pub fn transform_implementation_header(&mut self) {
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
        // Regex to match LLVM_LIBC_FUNCTION signature only, ignoring the body.
        // matches: LLVM_LIBC_FUNCTION(ret, name, (args))
        let re = Regex::new(r"LLVM_LIBC_FUNCTION\s*\(\s*([^,]+),\s*([^,]+),\s*\(([^)]*)\)\s*\)")
            .unwrap();

        if let Some(caps) = re.captures(text) {
            let ret_type = caps[1].trim();
            let name = caps[2].trim();
            let args = caps[3].trim();

            // We construct the new signature.
            // Note: We intentionally do NOT include 'static' as requested.
            // We only replace the matched range (the macro call), leaving the body (if present in node) alone.
            let new_sig = format!("LIBC_INLINE constexpr {} {}({})", ret_type, name, args);

            let match_range = caps.get(0).unwrap().range();
            let absolute_range =
                (node.start_byte() + match_range.start)..(node.start_byte() + match_range.end);

            self.edits.push(Edit {
                range: absolute_range,
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
                new_text: "LIBC_INLINE constexpr ".to_string(),
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
}

pub fn extract_and_create_support_header(
    func_name: &str,
    cpp_path: &Path,
    header_path: &Path,
    use_f16: bool,
    use_f128: bool,
    use_bf16: bool,
) -> Result<()> {
    let cpp_content = fs::read_to_string(cpp_path).context("Could not read generic CPP file")?;

    // 1. Transform structure using CppRefactorer (namespaces, static modifiers)
    // We transform the ENTIRE file content, preserving comments, tables, etc.
    let mut refactor = CppRefactorer::new(cpp_content.clone())?;
    refactor.transform_implementation_header();
    let transformed = refactor.apply();

    // 2. Post-process to make it a valid header (Guards, Includes, License)
    let func_upper = func_name.to_uppercase();
    let guard_name = format!("LLVM_LIBC_SRC___SUPPORT_MATH_{}_H", func_upper);

    let (macro_include, pre_guard, post_guard) = if use_f128 {
        (
            "#include \"include/llvm-libc-types/float128.h\"\n",
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
        // use_bf16 typically does not need guards or macro includes
        ("", "", "")
    };

    // Filter includes line by line
    let mut lines = Vec::new();
    for line in transformed.lines() {
        if line.starts_with("#include") {
            // Remove public header include
            if line.contains(&format!("src/math/{}.h", func_name)) {
                continue;
            }
            // Remove macros if we add them manually
            if (use_f16 && line.contains("float16-macros.h"))
                || (use_f128 && line.contains("float128-macros.h"))
            {
                continue;
            }
        }
        lines.push(line);
    }
    let body_content = lines.join("\n");
    let clean_body = remove_license_header(&body_content);

    let title = format!("Implementation header for {}", func_name);
    let prefix = format!("//===-- {} ", title);
    let suffix = "-*- C++ -*-===//";
    let remaining = 80usize.saturating_sub(prefix.len() + suffix.len());
    let dashes = "-".repeat(remaining);

    let header_line = format!("{}{}{}", prefix, dashes, suffix);

    let final_content = format!(
        r#"{header_line}
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef {guard}
#define {guard}

{macro_include}
{pre_guard}
{clean_body}
{post_guard}
#endif // {guard}
"#,
        header_line = header_line,
        guard = guard_name,
        macro_include = macro_include,
        pre_guard = pre_guard,
        post_guard = post_guard,
        clean_body = clean_body
    );

    fs::write(header_path, final_content)?;
    println!("Extracted implementation to: {:?}", header_path);
    Ok(())
}

fn remove_license_header(content: &str) -> String {
    let lines = content.lines();
    let mut result = Vec::new();
    let mut skipped_license = false;

    for line in lines {
        if !skipped_license {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.is_empty() {
                continue;
            }
            skipped_license = true;
        }
        result.push(line);
    }
    result.join("\n")
}

fn get_license_header(content: &str) -> String {
    let mut lines = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("//") || trimmed.is_empty() {
            lines.push(line);
            if trimmed.contains(
                "===----------------------------------------------------------------------===//",
            ) {
                break;
            }
        } else {
            break;
        }
    }
    // Remove trailing empty lines
    while let Some(last) = lines.last() {
        if last.trim().is_empty() {
            lines.pop();
        } else {
            break;
        }
    }
    lines.join("\n")
}

pub fn regenerate_generic_cpp(func_name: &str, cpp_path: &Path) -> Result<()> {
    let content = fs::read_to_string(cpp_path)?;

    // Parse signature from the original file
    let re =
        Regex::new(r"LLVM_LIBC_FUNCTION\s*\(\s*([^,]+),\s*([^,]+),\s*\(([^)]*)\)\s*\)").unwrap();

    if let Some(caps) = re.captures(&content) {
        let ret_type = caps[1].trim();
        // caps[2] is name (should match func_name)
        let args_def = caps[3].trim();

        // Parse arg names for the call
        let arg_names: Vec<&str> = args_def
            .split(',')
            .map(|arg| {
                arg.trim()
                    .split_whitespace()
                    .last()
                    .unwrap_or("")
                    .trim_start_matches(|c| c == '*' || c == '&')
            })
            .filter(|s| !s.is_empty())
            .collect();
        let call_args = arg_names.join(", ");

        let license_header = get_license_header(&content);

        let new_content = format!(
            r#"{license}

#include "src/math/{func}.h"
#include "src/__support/math/{func}.h"

namespace LIBC_NAMESPACE_DECL {{

LLVM_LIBC_FUNCTION({ret}, {func}, ({args})) {{
  return math::{func}({call});
}}

}} // namespace LIBC_NAMESPACE_DECL
"#,
            license = license_header,
            func = func_name,
            ret = ret_type,
            args = args_def,
            call = call_args
        );

        fs::write(cpp_path, new_content)?;
        println!("Regenerated generic CPP: {:?}", cpp_path);
    } else {
        println!("Could not parse LLVM_LIBC_FUNCTION signature in generic CPP to regenerate it.");
    }
    Ok(())
}

pub fn generate_shared_wrapper(
    func: &str,
    use_f16: bool,
    use_f128: bool,
    use_bf16: bool,
) -> String {
    let func_upper = func.to_uppercase();

    let (include_macro, pre_guard, post_guard) = if use_f128 {
        (
            "\n#include \"include/llvm-libc-types/float128.h\"\n",
            "\n#ifdef LIBC_TYPES_HAS_FLOAT128\n",
            "\n#endif // LIBC_TYPES_HAS_FLOAT128\n",
        )
    } else if use_f16 {
        (
            "\n#include \"include/llvm-libc-macros/float16-macros.h\"\n",
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

{include_macro}
{pre_guard}
#include "shared/libc_common.h"
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

pub fn update_shared_test_file(
    root: &Path,
    func: &str,
    args_str: &str,
    ret_type: &str,
) -> Result<()> {
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

    // 1. Map function suffix to TEST block and types
    let (block_name, zero_lit, base_type_decl, input_lit) = if func.ends_with("f16") {
        if func.ends_with("bf16") {
            ("AllBFloat16", "bfloat16(0.0)", "bfloat16", "bfloat16(0.0)")
        } else {
            ("AllFloat16", "0x0p+0f16", "float16", "0.0f16")
        }
    } else if func.ends_with("f128") {
        ("AllFloat128", "float128(0.0)", "float128", "float128(0.0)")
    } else if func.ends_with("f") {
        ("AllFloat", "0x0p+0f", "float", "0.0f")
    } else if func.ends_with("l") {
        ("AllLongDouble", "0x0p+0L", "long double", "0.0L")
    } else {
        ("AllDouble", "0.0", "double", "0.0")
    };

    // 2. Parse arguments to build setup code and call args
    let mut setup_lines = Vec::new();
    let mut call_args = Vec::new();
    let mut post_checks = Vec::new();

    if !args_str.trim().is_empty() {
        for arg in args_str.split(',') {
            let arg = arg.trim();
            if arg.is_empty() {
                continue;
            }

            // Determine type and initializer
            let (arg_type, arg_init, expect_val) = if arg.contains("bfloat16") {
                ("bfloat16", "bfloat16(0.0)", "bfloat16(0.0)")
            } else {
                (base_type_decl, input_lit, zero_lit)
            };

            // Simple parsing to detect pointers
            // We assume standard formatting e.g., "float *x" or "float* x"
            if arg.contains('*') {
                // Pointer argument
                // Extract name: last word, cleaned of * and &
                let parts: Vec<&str> = arg.split_whitespace().collect();
                let raw_name = parts
                    .last()
                    .unwrap_or(&"val")
                    .trim_start_matches(|c| c == '*' || c == '&');

                // Prefix variable name with function name to avoid collisions
                let var_name = format!("{}_{}", func, raw_name);

                // Declare local variable
                setup_lines.push(format!("  {} {} = {};", arg_type, var_name, arg_init));
                // Pass address
                call_args.push(format!("&{}", var_name));

                // Add post-call check ONLY if not const
                if !arg.contains("const") {
                    post_checks.push(format!("  EXPECT_FP_EQ({}, {});", expect_val, var_name));
                }
            } else {
                // Scalar argument - pass literal
                // If it's bfloat16 scalar, use the init value
                call_args.push(arg_init.to_string());
            }
        }
    }

    // 3. Handle return type and invocation
    let call_expr = format!("LIBC_NAMESPACE::shared::{}({})", func, call_args.join(", "));
    let invocation_line = if ret_type.contains("void") {
        format!("  {};", call_expr)
    } else if ret_type.contains("int") {
        format!("  EXPECT_EQ(0, {});", call_expr)
    } else {
        format!("  EXPECT_FP_EQ({}, {});", zero_lit, call_expr)
    };

    // 4. Combine lines
    let mut test_block = String::new();
    for line in setup_lines {
        test_block.push_str(&line);
        test_block.push('\n');
    }
    test_block.push_str(&invocation_line);
    test_block.push('\n');
    for line in post_checks {
        test_block.push_str(&line);
        test_block.push('\n');
    }

    // 5. Inject into the correct TEST block
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
        new_body.push_str(&test_block);
        // Ensure trailing newline isn't duplicated excessively, but push one for safety
        if !new_body.ends_with('\n') {
            new_body.push('\n');
        }

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
