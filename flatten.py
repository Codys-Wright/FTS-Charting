#!/usr/bin/env python3
import re
import sys

def flatten_includes(content, base_dir=""):
    """Recursively flatten \include statements"""
    lines = content.split("\n")
    result = []
    
    for line in lines:
        # Match \include "filename.ly"
        match = re.match(r"\include\s+\"([^\"]+)\"", line.strip())
        if match:
            filename = match.group(1)
            if base_dir:
                filename = f"{base_dir}/{filename}"
            
            try:
                with open(filename, "r") as f:
                    included_content = f.read()
                    # Recursively flatten includes in the included file
                    flattened_content = flatten_includes(included_content, base_dir)
                    result.append(f"% Included from {filename}")
                    result.append(flattened_content)
                    result.append(f"% End of {filename}")
            except FileNotFoundError:
                result.append(f"% ERROR: Could not find {filename}")
                result.append(line)
        else:
            result.append(line)
    
    return "\n".join(result)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 flatten.py <input.ly>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    with open(input_file, "r") as f:
        content = f.read()
    
    flattened = flatten_includes(content)
    print(flattened)
