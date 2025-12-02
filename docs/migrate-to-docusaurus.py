#!/usr/bin/env python3
"""
Migrate Mintlify MDX files to Docusaurus format
"""

import os
import re
import shutil
import json
from pathlib import Path

MINTLIFY_DIR = Path("mintlify")
DOCUSAURUS_DIR = Path("docusaurus")
DOCS_DIR = DOCUSAURUS_DIR / "docs"

# Mapping of Mintlify components to Docusaurus equivalents
def convert_mintlify_to_docusaurus(content: str, filepath: str) -> str:
    """Convert Mintlify MDX components to Docusaurus format"""

    # Remove Mintlify-specific image components with className
    content = re.sub(
        r'<img\s+className="[^"]*"\s+src="([^"]*)"\s+alt="([^"]*)"\s*/?>',
        r'![\2](\1)',
        content
    )

    # Convert CardGroup to div with grid
    content = re.sub(
        r'<CardGroup\s+cols=\{(\d+)\}>',
        r'<div className="card-group cols-\1">',
        content
    )
    content = content.replace('</CardGroup>', '</div>')

    # Convert Card to Docusaurus-style link cards
    def convert_card(match):
        title = match.group(1)
        icon = match.group(2) if match.group(2) else ""
        href = match.group(3)
        description = match.group(4).strip()
        return f'''<div className="card">
  <a href="{href}">
    <h3>{title}</h3>
    <p>{description}</p>
  </a>
</div>'''

    content = re.sub(
        r'<Card\s+title="([^"]*)"\s+icon="([^"]*)"\s+href="([^"]*)"\s*>\s*(.*?)\s*</Card>',
        convert_card,
        content,
        flags=re.DOTALL
    )

    # Convert AccordionGroup to details
    content = content.replace('<AccordionGroup>', '')
    content = content.replace('</AccordionGroup>', '')

    # Convert Accordion to details/summary
    def convert_accordion(match):
        title = match.group(1)
        body = match.group(2).strip()
        return f'''<details>
<summary>{title}</summary>

{body}

</details>'''

    content = re.sub(
        r'<Accordion\s+title="([^"]*)"\s*>\s*(.*?)\s*</Accordion>',
        convert_accordion,
        content,
        flags=re.DOTALL
    )

    # Convert Tabs to Docusaurus Tabs
    content = content.replace('<Tabs>', '\nimport Tabs from \'@theme/Tabs\';\nimport TabItem from \'@theme/TabItem\';\n\n<Tabs>')
    content = re.sub(r'<Tab\s+title="([^"]*)">',r'<TabItem value="\1" label="\1">', content)
    content = content.replace('</Tab>', '</TabItem>')

    # Convert Frame to figure
    content = re.sub(r'<Frame>', '<figure>', content)
    content = re.sub(r'</Frame>', '</figure>', content)

    # Convert Tip to Docusaurus admonition
    content = re.sub(r'<Tip>', ':::tip', content)
    content = re.sub(r'</Tip>', ':::', content)

    # Convert Warning to Docusaurus admonition
    content = re.sub(r'<Warning>', ':::warning', content)
    content = re.sub(r'</Warning>', ':::', content)

    # Convert Note to Docusaurus admonition
    content = re.sub(r'<Note>', ':::note', content)
    content = re.sub(r'</Note>', ':::', content)

    # Convert Info to Docusaurus admonition
    content = re.sub(r'<Info>', ':::info', content)
    content = re.sub(r'</Info>', ':::', content)

    # Convert CodeGroup to Docusaurus code tabs
    content = re.sub(r'<CodeGroup>', '\nimport CodeBlock from \'@theme/CodeBlock\';\nimport Tabs from \'@theme/Tabs\';\nimport TabItem from \'@theme/TabItem\';\n\n<Tabs>', content)
    content = content.replace('</CodeGroup>', '</Tabs>')

    # Convert code blocks with titles in CodeGroup
    def convert_codeblock_in_group(match):
        lang = match.group(1)
        title = match.group(2)
        code = match.group(3)
        return f'''<TabItem value="{title}" label="{title}">

```{lang}
{code}
```

</TabItem>'''

    content = re.sub(
        r'```(\w+)\s+([^\n]+)\n(.*?)```',
        convert_codeblock_in_group,
        content,
        flags=re.DOTALL
    )

    # Fix image paths
    content = content.replace('src="/images/', 'src="/img/')
    content = content.replace('](/images/', '](/img/')

    # Fix internal links to use .md extension for Docusaurus
    content = re.sub(r'href="/([^"#]+)"', lambda m: f'href="/{m.group(1)}"', content)

    return content


def get_sidebar_position(filepath: str) -> int:
    """Determine sidebar position based on file path"""
    positions = {
        'introduction': 1,
        'quickstart': 2,
        'architecture': 3,
        'overview': 1,
    }
    filename = Path(filepath).stem
    return positions.get(filename, 99)


def process_file(src_path: Path, dest_path: Path):
    """Process a single MDX file"""
    with open(src_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Convert content
    converted = convert_mintlify_to_docusaurus(content, str(src_path))

    # Add sidebar_position to frontmatter if not present
    if '---' in converted:
        parts = converted.split('---', 2)
        if len(parts) >= 3:
            frontmatter = parts[1]
            if 'sidebar_position' not in frontmatter:
                position = get_sidebar_position(str(src_path))
                frontmatter = frontmatter.rstrip() + f'\nsidebar_position: {position}\n'
            converted = f'---{frontmatter}---{parts[2]}'

    # Ensure destination directory exists
    dest_path.parent.mkdir(parents=True, exist_ok=True)

    # Write converted file
    with open(dest_path, 'w', encoding='utf-8') as f:
        f.write(converted)

    print(f"  Converted: {src_path} -> {dest_path}")


def main():
    print("Migrating Mintlify to Docusaurus...")

    # Create docs directory structure
    DOCS_DIR.mkdir(parents=True, exist_ok=True)

    # Copy images
    mintlify_images = MINTLIFY_DIR / "images"
    docusaurus_images = DOCUSAURUS_DIR / "static" / "img"
    if mintlify_images.exists():
        if docusaurus_images.exists():
            shutil.rmtree(docusaurus_images)
        shutil.copytree(mintlify_images, docusaurus_images)
        print(f"  Copied images to {docusaurus_images}")

    # Copy logo
    mintlify_logo = MINTLIFY_DIR / "logo"
    if mintlify_logo.exists():
        for logo_file in mintlify_logo.glob("*"):
            shutil.copy(logo_file, docusaurus_images / logo_file.name)
        print(f"  Copied logos to {docusaurus_images}")

    # Process all MDX files
    mdx_files = list(MINTLIFY_DIR.glob("**/*.mdx"))
    print(f"\nFound {len(mdx_files)} MDX files to migrate\n")

    for src_path in mdx_files:
        # Calculate relative path
        rel_path = src_path.relative_to(MINTLIFY_DIR)
        dest_path = DOCS_DIR / rel_path

        process_file(src_path, dest_path)

    print(f"\n✅ Migration complete! {len(mdx_files)} files converted")
    print(f"\nNext steps:")
    print(f"  1. cd docusaurus")
    print(f"  2. npm start")
    print(f"  3. Review and fix any component issues")


if __name__ == "__main__":
    main()
