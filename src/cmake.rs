use anyhow::Result;
use regex::RegexBuilder;
use std::fs;
use std::path::Path;

pub fn update_cmake_files(root: &Path, func: &str) -> Result<()> {
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
        if original.contains("libc.src.__support.")
            || original.contains("libc.hdr.")
            || original.contains("libc.src.errno.errno")
            || (original.contains("libc.src.math.") && original.contains("_constants"))
        {
            deps.push(original);
        }
    }
    deps.sort();
    deps.dedup();

    let deps_str = deps.join("\n    ");
    let new_block = format!(
        r#"add_header_library(
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

    // Find insertion point (alphabetical order of targets)
    let re = RegexBuilder::new(r"add_header_library\(\s*([a-zA-Z0-9_]+)")
        .multi_line(true)
        .build()
        .unwrap();
    let mut insert_pos = content.len(); // Default to end
    let mut found_pos = false;

    for cap in re.captures_iter(&content) {
        let target_name = &cap[1];
        if func < target_name {
            if let Some(m) = cap.get(0) {
                insert_pos = m.start();
                // Move back to include any preceding blank lines or comments if needed
                let prefix = &content[..insert_pos];
                if let Some(last_newline) = prefix.rfind('\n') {
                    // Check if the line before is empty, if so insert before that
                    if last_newline > 0 && prefix[last_newline - 1..last_newline].trim().is_empty()
                    {
                        insert_pos = last_newline; // Before the blank line
                    }
                }
                found_pos = true;
                break;
            }
        }
    }

    if found_pos {
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
        let replacement = format!("$1\n    libc.src.__support.math.{}\n$3", func);
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

    // Find the shared_math_test block
    // Use a regex that captures the DEPENDS block content but STOPS at the closing parenthesis of the command.
    // The previous regex might have been too greedy or imprecise.
    // We want: add_fp_unittest( ... DEPENDS ... )
    // We target the "DEPENDS" keyword and capture everything until the next ')' that isn't part of the content.
    // Assuming simple structure where ')' closes the command.
    let re_block = RegexBuilder::new(
        r"(add_fp_unittest\s*\(\s*shared_math_test\s*[\s\S]*?DEPENDS\s*)([\s\S]*?)(\))",
    )
    .multi_line(true)
    .build()
    .unwrap();

    if let Some(cap) = re_block.captures(&content) {
        let prefix = &cap[1];
        let block_content = &cap[2];
        let suffix = &cap[3];
        let full_match_range = cap.get(0).unwrap().range();

        // Ensure we preserve the surrounding structure.
        let ends_with_newline = block_content.ends_with('\n');

        let lines: Vec<&str> = block_content.lines().collect();
        let mut new_lines = Vec::new();
        let mut inserted = false;

        for line in lines {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                // Keep empty lines for spacing
                new_lines.push(line.to_string());
                continue;
            }

            if !inserted && trimmed > &target_dep {
                new_lines.push(format!("    {}", target_dep));
                inserted = true;
            }
            new_lines.push(line.to_string());
        }

        if !inserted {
            new_lines.push(format!("    {}", target_dep));
        }

        // Reconstruct block
        let mut new_depends_block = new_lines.join("\n");

        // Handle trailing newline logic
        if ends_with_newline && !new_depends_block.ends_with('\n') {
            new_depends_block.push('\n');
        } else if !ends_with_newline {
            // If original didn't end with newline, we generally want one if we added multiple lines
            if new_lines.len() > 1 && !new_depends_block.ends_with('\n') {
                new_depends_block.push('\n');
            }
        }

        // Replace the WHOLE match with prefix + new_content + suffix
        // This ensures we reconstruct the command exactly as it was, just with different DEPENDS content
        let new_full_block = format!("{}{}{}", prefix, new_depends_block, suffix);

        content.replace_range(full_match_range, &new_full_block);

        fs::write(cmake_path, content)?;
        println!("  Updated: {:?}", cmake_path);
        return Ok(());
    }

    println!("  Warning: Could not automatically insert into test CMake.");
    Ok(())
}
