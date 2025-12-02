#!/usr/bin/env python3
"""
Fix MDX files for Docusaurus compatibility - remove problematic components
"""

import os
import re
from pathlib import Path

DOCS_DIR = Path("docs")


def fix_mdx_content(content: str) -> str:
    """Fix MDX content for Docusaurus compatibility"""

    # Remove all import statements that were incorrectly inserted
    content = re.sub(r"^import\s+.*?from\s+['\"].*?['\"];?\s*$", "", content, flags=re.MULTILINE)

    # Remove Tabs/TabItem components and convert to simple sections
    content = re.sub(r"<Tabs>", "", content)
    content = re.sub(r"</Tabs>", "", content)
    content = re.sub(r'<TabItem\s+value="([^"]*)"\s+label="([^"]*)">', r"\n### \2\n", content)
    content = re.sub(r"</TabItem>", "", content)

    # Remove CodeBlock imports and components
    content = re.sub(r"<CodeBlock[^>]*>", "", content)
    content = re.sub(r"</CodeBlock>", "", content)

    # Fix div with className - convert to simple divs
    content = re.sub(r'<div className="[^"]*">', "<div>", content)

    # Remove card-group divs entirely - keep content
    content = re.sub(r'<div className="card-group[^"]*">', "", content)
    content = re.sub(r"</div>\s*</div>", "</div>", content)  # Clean up nested divs

    # Convert card divs to markdown
    def convert_card_to_md(match):
        href = match.group(1)
        title = match.group(2)
        desc = match.group(3)
        return f"- **[{title}]({href})**: {desc}\n"

    content = re.sub(
        r'<div className="card">\s*<a href="([^"]*)">\s*<h3>([^<]*)</h3>\s*<p>([^<]*)</p>\s*</a>\s*</div>',
        convert_card_to_md,
        content,
        flags=re.DOTALL,
    )

    # Clean up remaining card divs
    content = re.sub(r'<div className="card">', "", content)
    content = re.sub(r"<div>[\s\n]*</div>", "", content)

    # Fix figure tags
    content = re.sub(r"<figure>", "", content)
    content = re.sub(r"</figure>", "", content)

    # Clean up extra blank lines
    content = re.sub(r"\n{4,}", "\n\n\n", content)

    # Remove stray closing divs
    content = re.sub(r"^\s*</div>\s*$", "", content, flags=re.MULTILINE)

    return content


def process_file(filepath: Path):
    """Process a single MDX file"""
    with open(filepath, "r", encoding="utf-8") as f:
        content = f.read()

    fixed = fix_mdx_content(content)

    if content != fixed:
        with open(filepath, "w", encoding="utf-8") as f:
            f.write(fixed)
        print(f"  Fixed: {filepath}")
    else:
        print(f"  OK: {filepath}")


def main():
    print("Fixing MDX files for Docusaurus compatibility...\n")

    mdx_files = list(DOCS_DIR.glob("**/*.mdx"))
    print(f"Found {len(mdx_files)} MDX files\n")

    for filepath in mdx_files:
        process_file(filepath)

    print(f"\n✅ Done! Fixed {len(mdx_files)} files")


if __name__ == "__main__":
    main()
