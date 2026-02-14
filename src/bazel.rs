use anyhow::Result;
use regex::RegexBuilder;
use std::fs;
use std::path::Path;

pub fn update_bazel_files(root: &Path, func: &str) -> Result<()> {
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

    // Check if target already exists (handle both quoted and unquoted variations just in case, though Bazel uses quotes)
    if content.contains(&format!("name = \"{}\"", target_name)) {
        println!("  Skipping Bazel support target (already exists)");
        return Ok(());
    }

    let deps = get_bazel_deps_from_cmake(cmake_path, &content, func)?;
    let deps_str = deps
        .iter()
        .map(|d| format!("\"{}\",", d))
        .collect::<Vec<_>>()
        .join("\n        ");

    let new_block = format!(
        r#"libc_support_library(
    name = "{}",
    hdrs = ["src/__support/math/{}.h"],
    deps = [
        {}
    ],
)
"#,
        target_name, func, deps_str
    );

    // Determine insertion position based on CMake order
    let insert_pos = find_insertion_point_from_cmake(&content, cmake_path, func)?;

    if insert_pos < content.len() {
        content.insert_str(insert_pos, &new_block);
        // Ensure separation
        if !content[insert_pos..].starts_with('\n') {
            content.insert(insert_pos + new_block.len(), '\n');
        }
    } else {
        if !content.ends_with('\n') {
            content.push('\n');
        }
        content.push_str(&new_block);
    }

    fs::write(bazel_path, content)?;
    println!("  Added support library to Bazel");
    Ok(())
}

fn find_insertion_point_from_cmake(
    bazel_content: &str,
    cmake_path: &Path,
    func: &str,
) -> Result<usize> {
    // 1. Read CMake file and find the function BEFORE the current one (predecessor)
    let cmake_content = fs::read_to_string(cmake_path)?;

    // Find all add_header_library targets in order
    let re_cmake = RegexBuilder::new(r"add_header_library\(\s*([a-zA-Z0-9_]+)")
        .multi_line(true)
        .build()
        .unwrap();

    let mut prev_target = None;
    for cap in re_cmake.captures_iter(&cmake_content) {
        let current_target = &cap[1];
        if current_target == func {
            break;
        }
        prev_target = Some(current_target.to_string());
    }

    // 2. If we found a predecessor, find its block in Bazel and insert AFTER it
    if let Some(prev) = prev_target {
        let prev_bazel_name = format!("__support_math_{}", prev);
        let pattern = format!(
            r#"libc_support_library\(\s*name\s*=\s*"{}"[\s\S]*?"#,
            regex::escape(&prev_bazel_name)
        );
        let re_bazel = RegexBuilder::new(&pattern)
            .multi_line(true)
            .build()
            .unwrap();

        if let Some(m) = re_bazel.find(bazel_content) {
            // Found the start of the predecessor block. Now find its end.
            if let Some(end_pos) = find_block_end(bazel_content, m.start()) {
                // Insert after the closing parenthesis and potentially following newline
                let mut pos = end_pos;
                // Skip subsequent newlines to be clean
                while pos < bazel_content.len() && bazel_content[pos..].starts_with('\n') {
                    pos += 1;
                }
                return Ok(pos);
            }
        }
        println!(
            "  Warning: Predecessor {} found in CMake but not in Bazel. Falling back to append.",
            prev
        );
    }

    // 3. If no predecessor (it's the first one) OR predecessor not found in Bazel:
    // Try to find the start of the support library section
    if let Some(pos) = bazel_content
        .find("########################### math support library ###############################")
    {
        let start_pos = pos
            + "########################### math support library ###############################"
                .len();
        // find next newline
        if let Some(nl) = bazel_content[start_pos..].find('\n') {
            return Ok(start_pos + nl + 1);
        }
        return Ok(start_pos);
    }

    // 4. Fallback: Append to end of file if section not found
    Ok(bazel_content.len())
}

fn find_block_end(content: &str, start: usize) -> Option<usize> {
    // Basic parenthesis counter
    let mut open = 0;
    let mut found_start = false;
    let mut chars = content[start..].char_indices();

    while let Some((idx, c)) = chars.next() {
        if c == '(' {
            open += 1;
            found_start = true;
        } else if c == ')' {
            open -= 1;
            if found_start && open == 0 {
                // Return absolute position of ')' + 1
                return Some(start + idx + 1);
            }
        }
    }
    None
}

fn update_bazel_math_function_target(bazel_path: &Path, func: &str) -> Result<()> {
    let content = fs::read_to_string(bazel_path)?;
    let support_target = format!(":__support_math_{}", func);

    // 1. Try to find multi-line definition with additional_deps
    let pattern_multiline = format!(
        r#"(libc_math_function\s*\(\s*name\s*=\s*"{}"\s*,[\s\S]*?additional_deps\s*=\s*\[)([\s\S]*?)(\])"#,
        regex::escape(func)
    );
    let re_multiline = RegexBuilder::new(&pattern_multiline)
        .multi_line(true)
        .build()
        .unwrap();

    if re_multiline.is_match(&content) {
        let replacement = format!("$1\n        \"{}\"\n    $3", support_target);
        let new_content = re_multiline.replace(&content, replacement).to_string();
        fs::write(bazel_path, new_content)?;
        println!("  Updated additional_deps for {} in Bazel", func);
        return Ok(());
    }

    // 2. Try to find one-liner definition or block without additional_deps
    let pattern_oneliner = format!(
        r#"libc_math_function\s*\(\s*name\s*=\s*"{}"\s*,?\s*\)"#,
        regex::escape(func)
    );
    let re_oneliner = RegexBuilder::new(&pattern_oneliner)
        .multi_line(true)
        .build()
        .unwrap();

    if re_oneliner.is_match(&content) {
        let replacement = format!(
            r#"libc_math_function(
    name = "{}",
    additional_deps = [
        "{}",
    ],
)"#,
            func, support_target
        );
        let new_content = re_oneliner.replace(&content, replacement).to_string();
        fs::write(bazel_path, new_content)?;
        println!("  Expanded one-liner for {} in Bazel", func);
        return Ok(());
    }

    println!(
        "  Warning: Could not find libc_math_function definition for {}",
        func
    );
    Ok(())
}

fn get_bazel_deps_from_cmake(
    cmake_path: &Path,
    bazel_content: &str,
    func: &str,
) -> Result<Vec<String>> {
    let content = fs::read_to_string(cmake_path)?;
    let mut deps = Vec::new();

    // Find the add_header_library block for this function and capture DEPENDS
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

            // Map CMake targets to Bazel targets
            if dep == "libc.src.errno.errno" {
                deps.push(":errno".to_string());
            } else if dep.contains("_constants") {
                if let Some(name) = dep.split('.').last() {
                    deps.push(format!(":{}", name));
                }
            } else if dep.starts_with("libc.src.__support.") {
                // Try to find the target by searching for the header file in the Bazel file

                // Remove "libc." prefix
                let path_part = dep.strip_prefix("libc.").unwrap_or(dep);
                // Convert remaining dots to slashes and append .h
                let header_path = path_part.replace('.', "/") + ".h";

                // Now search for this header path in the Bazel content
                let mut found_target_name = None;

                if let Some(idx) = bazel_content.find(&header_path) {
                    // Found the header path at idx.
                    // We scan backwards from idx to find the nearest `name = "..."`
                    // This assumes the `name` attribute comes BEFORE `hdrs`.

                    let prefix = &bazel_content[..idx];
                    // Search for `name = "` backwards
                    if let Some(name_idx) = prefix.rfind("name = \"") {
                        // Extract the name
                        let name_start = name_idx + "name = \"".len();
                        if let Some(name_end) = prefix[name_start..].find('"') {
                            let extracted_name = &prefix[name_start..name_start + name_end];
                            found_target_name = Some(extracted_name.to_string());
                        }
                    }
                }

                if let Some(name) = found_target_name {
                    deps.push(format!(":{}", name));
                } else {
                    // Fallback to heuristic if not found
                    let parts: Vec<&str> = dep.split('.').collect();
                    if parts.len() >= 5 {
                        let suffix = parts[3..].join("_").to_lowercase();
                        deps.push(format!(":__support_{}", suffix));
                    }
                }
            } else if dep.starts_with("libc.hdr.") {
                // Map header dependencies if needed
            }
        }
    }

    deps.sort();
    deps.dedup();
    Ok(deps)
}
