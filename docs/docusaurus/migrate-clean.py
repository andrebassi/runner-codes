#!/usr/bin/env python3
"""
Clean migration from Mintlify to Docusaurus
Preserves all content but fixes formatting issues
"""

import os
import re
import shutil

MINTLIFY_DIR = "/Users/andrebassi/works/.solutions/runner.codes/llm-firecracker/docs/mintlify"
DOCUSAURUS_DIR = "/Users/andrebassi/works/.solutions/runner.codes/llm-firecracker/docs/docusaurus/docs"

def clean_mdx_content(content, filename):
    """Clean MDX content for Docusaurus compatibility"""

    # Remove Mintlify-specific components
    content = re.sub(r'<Card[^>]*>.*?</Card>', '', content, flags=re.DOTALL)
    content = re.sub(r'<CardGroup[^>]*>.*?</CardGroup>', '', content, flags=re.DOTALL)
    content = re.sub(r'<Tabs[^>]*>', '', content)
    content = re.sub(r'</Tabs>', '', content)
    content = re.sub(r'<Tab[^>]*>', '', content)
    content = re.sub(r'</Tab>', '', content)
    content = re.sub(r'<TabItem[^>]*>', '', content)
    content = re.sub(r'</TabItem>', '', content)
    content = re.sub(r'<Steps[^>]*>', '', content)
    content = re.sub(r'</Steps>', '', content)
    content = re.sub(r'<Step[^>]*>', '', content)
    content = re.sub(r'</Step>', '', content)
    content = re.sub(r'<Accordion[^>]*>', '', content)
    content = re.sub(r'</Accordion>', '', content)
    content = re.sub(r'<AccordionGroup[^>]*>', '', content)
    content = re.sub(r'</AccordionGroup>', '', content)
    content = re.sub(r'<Frame[^>]*>', '', content)
    content = re.sub(r'</Frame>', '', content)
    content = re.sub(r'<CodeGroup[^>]*>', '', content)
    content = re.sub(r'</CodeGroup>', '', content)
    content = re.sub(r'<ResponseField[^>]*>.*?</ResponseField>', '', content, flags=re.DOTALL)
    content = re.sub(r'<ParamField[^>]*>.*?</ParamField>', '', content, flags=re.DOTALL)

    # Convert details/summary to markdown
    content = re.sub(r'<details>\s*<summary>([^<]+)</summary>', r'**\1**\n', content)
    content = re.sub(r'</details>', '', content)

    # Remove empty divs
    content = re.sub(r'<div[^>]*>\s*</div>', '', content)
    content = re.sub(r'<div[^>]*>', '', content)
    content = re.sub(r'</div>', '', content)

    # Remove empty anchor tags
    content = re.sub(r'<a[^>]*>\s*</a>', '', content)

    # Convert Mintlify callouts to Docusaurus admonitions
    content = re.sub(r'<Note>', ':::note\n', content)
    content = re.sub(r'</Note>', '\n:::', content)
    content = re.sub(r'<Warning>', ':::warning\n', content)
    content = re.sub(r'</Warning>', '\n:::', content)
    content = re.sub(r'<Tip>', ':::tip\n', content)
    content = re.sub(r'</Tip>', '\n:::', content)
    content = re.sub(r'<Info>', ':::info\n', content)
    content = re.sub(r'</Info>', '\n:::', content)

    # Fix code blocks - ensure they have proper formatting
    # Remove any ### headers that are just commands
    content = re.sub(r'^### (#.*|sudo .*|infra\..*|curl .*|\{)$', '', content, flags=re.MULTILINE)

    # Clean up multiple blank lines
    content = re.sub(r'\n{3,}', '\n\n', content)

    # Clean up leading/trailing whitespace on lines
    lines = content.split('\n')
    cleaned_lines = []
    for line in lines:
        # Don't strip lines inside code blocks
        cleaned_lines.append(line.rstrip())
    content = '\n'.join(cleaned_lines)

    return content

def migrate_file(src_path, dst_path):
    """Migrate a single file"""
    with open(src_path, 'r', encoding='utf-8') as f:
        content = f.read()

    filename = os.path.basename(src_path)
    content = clean_mdx_content(content, filename)

    # Change extension from .mdx to .md
    if dst_path.endswith('.mdx'):
        dst_path = dst_path[:-4] + '.md'

    os.makedirs(os.path.dirname(dst_path), exist_ok=True)

    with open(dst_path, 'w', encoding='utf-8') as f:
        f.write(content)

    print(f"Migrated: {src_path} -> {dst_path}")

def main():
    # Walk through mintlify directory
    for root, dirs, files in os.walk(MINTLIFY_DIR):
        for file in files:
            if file.endswith('.mdx') or file.endswith('.md'):
                src_path = os.path.join(root, file)
                rel_path = os.path.relpath(src_path, MINTLIFY_DIR)
                dst_path = os.path.join(DOCUSAURUS_DIR, rel_path)

                # Change extension to .md
                if dst_path.endswith('.mdx'):
                    dst_path = dst_path[:-4] + '.md'

                migrate_file(src_path, dst_path)

    print(f"\nMigration complete! Files in {DOCUSAURUS_DIR}")

if __name__ == "__main__":
    main()
