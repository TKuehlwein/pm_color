import os

# Get the directory of the current script
script_dir = os.path.dirname(os.path.abspath(__file__))

# Change the current working directory
os.chdir(script_dir)

def generate_preload_tags(directory, output_file='preload_tags.txt'):
    """
    Generate HTML preload tags for image files in the specified directory and save them to a text file.

    :param directory: Directory containing the image files.
    :param output_file: Name of the text file to save the preload tags.
    """
    # List of image file extensions to look for
    valid_extensions = ('.jpg', '.jpeg', '.png', '.gif', '.webp')
    
    # Open the output file in write mode
    with open(output_file, 'w') as file:
        # Iterate through all files in the specified directory
        for filename in os.listdir(directory):
            # Check if the file is an image based on its extension
            if filename.lower().endswith(valid_extensions):
                # Write the preload tag to the file
                file.write(f'<link rel="preload" as="image" href="https://memcog.fernuni.ch/studies/prm_wm_s1/images/{filename}">\n')

if __name__ == "__main__":
    # Specify the directory containing your images
    image_directory = 'images/'
    
    # Specify the name of the output file (optional)
    output_filename = 'preload_tags.txt'
    
    # Generate the preload tags and save them to the file
    generate_preload_tags(image_directory, output_filename)

    print(f"Preload tags have been saved to {output_filename}.")
