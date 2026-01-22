'''
Script Name: Disease Cluster Analysis Data Export Input Processing
Author: Nicholas Hollingshead, Cornell University
Description: Prepares data from the CWD Data Warehouse for the Disease Cluster Analysis Data Export R script.
Inputs: 
  sample.ndjson
  params.json
Outputs: 
  samples.csv
  params.csv
  info.html
  execution_log.log 
'''

##############
# Environment
import sys
import os
import ndjson
import json
import pathlib
import csv
import logging
import traceback
import datetime

##################
# SCRIPT VARIABLES

if os.name == 'nt':  # Windows
  base_path = pathlib.Path("data")
else: # Assuming Linux/Docker
  base_path = pathlib.Path("/data")

parameters_file_path = base_path / "params.json"
sample_file_path = base_path / "sample.ndJson"
model_metadata_log_file = base_path / "attachments" / "info.html"
logging_path = base_path / "attachments" / "execution_log.log"
attachments_json_path = base_path / "attachments.json"

###################
# FUNCTIONS

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
    """
    Writes a single line to the model_metadata_log text file with specified HTML element.

    Args:
        line: The line to be written.
        filename: The name of the file.
        html_element: The HTML element tag to use (e.g., "h1", "h2", "p", "div").
    """
    with open(filename, 'a') as f:
        f.write(f"<{html_element}>{line}</{html_element}>" + '\n')

def dict_to_html_list(data, list_type='unordered'):
  """
  Converts a Python dictionary to an HTML string representing a list.

  Args:
    data: The input dictionary.
    list_type: 'unordered' (default) or 'ordered' to specify the list type.

  Returns:
    An HTML string representing the dictionary.
  """

  def _dict_to_html_helper(data):
    """Recursive helper function to handle nested dictionaries."""
    html_str = ""
    if list_type == 'unordered':
      html_str += "<ul>"
    elif list_type == 'ordered':
      html_str += "<ol>"
    else:
      raise ValueError("Invalid list_type. Use 'unordered' or 'ordered'.")

    for key, value in data.items():
      html_str += f"<li>{key}: "
      if isinstance(value, dict):
        html_str += _dict_to_html_helper(value)
      elif isinstance(value, list):
        html_str += "<ul>"
        for item in value:
          html_str += f"<li>{item}</li>"
        html_str += "</ul>"
      else:
        html_str += f"{value}"
      html_str += "</li>"

    if list_type == 'unordered':
      html_str += "</ul>"
    elif list_type == 'ordered':
      html_str += "</ol>"

    return html_str

  return _dict_to_html_helper(data)

def rename_key(dict_, old_key, new_key):
  """Renames a key in a dictionary.

  Args:
    dict_: The dictionary to modify.
    old_key: The key to be renamed.
    new_key: The new key name.
  """

  if old_key in dict_:
    dict_[new_key] = dict_.pop(old_key)
    
def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2) 

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise

def add_missing_defaults_inplace(original_dict, default_values):
  """
  Adds missing key-value pairs from a default values dictionary to the original dictionary 
  in-place.

  Args:
    original_dict: The dictionary to which default values will be added.
    default_values: A dictionary containing default key-value pairs.
  """

  for key, value in default_values.items():
    if key not in original_dict:
      original_dict[key] = value

######################
# SETUP FILE STRUCTURE

# Create the attachments directory structure recursively if it doesn't already exist.
os.makedirs(os.path.dirname(model_metadata_log_file), exist_ok=True)

# Create attachments.json file which will contain a list of all attachments generated
# Initially, the attachments is simply an empty list
with open(attachments_json_path, 'w', newline='') as f:
  writer = json.dump(list(), f)

# Append execution log to attachments.json for developer feedback
attachment = {
  "filename": "execution_log.log", 
  "content_type": "text/plain", 
  "role": "downloadable"
  }
add_item_to_json_file_list(attachments_json_path, attachment)

# append info log to the attachments.json for user feedback
attachment = {
  "filename": "info.html", 
  "content_type": "text/html", 
  "role": "feedback"}
add_item_to_json_file_list(attachments_json_path, attachment)

###############
# SETUP LOGGING

# Create log file including any parent folders (if they don't already exist)
os.makedirs(os.path.dirname(logging_path), exist_ok=True)

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'w', # a is append, w is overbite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(exc_type, exc_value, exc_traceback):
  """
  Handles uncaught exceptions by logging the traceback and other details.

  Args:
    exc_type: The type of the exception.
    exc_value: The exception instance.
    exc_traceback: The traceback object.
  """
  logging.error("Uncaught exception:", exc_info=(exc_type, exc_value, exc_traceback))
sys.excepthook = handle_uncaught_exception 

# Initiate model metadata log

# Clear model log file contents if necessary.
open(model_metadata_log_file, 'w').close()
model_log_html("Model Execution Summary", "h3")
model_log_html("Model: Disease Cluster Analysis Data Export")
model_log_html('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("Model: Disease Cluster Analysis Data Export")
logging.info('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("This log records data for debugging purposes in the case of a model execution error.")

####################
# Process Parameters

# Get model parameters file
try:
  with open(parameters_file_path, 'r') as f:
    params = json.load(f)
    logging.info("Parameter file loaded successfully")
except FileNotFoundError:
  # The model cannot be executed without a params file. Exit with an error immediately.
  logging.error("params.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Parameters (params.json) file not found.")
  sys.exit(1)

# Get provider admin area
provider_admin_area = params['_provider']['_administrative_area']['administrative_area']
# Remove Provider parameter, which is not used in this model and is nested
del(params['_provider'])

# Merge the list of season_year values into a single string
if 'season_year' in params:
  params['season_year'] = ', '.join(params['season_year'])

# Write revised parameters to a CSV file
with open(pathlib.Path(base_path / "params.csv"), 'w', newline='') as f:
  field_names = params.keys()
  writer = csv.DictWriter(
    f, 
    quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=field_names, 
    extrasaction='ignore')
  writer.writeheader()
  writer.writerow(params)
  
# Add parameter related content to the log
model_log_html(f'Provider area: {provider_admin_area}')
model_log_html('User provided parameters', "h4")
model_log_html(dict_to_html_list(params))

#################
# Process Samples

try:
  with open(sample_file_path, 'r') as f:
    samples = ndjson.load(f)
    logging.info("Sample file loaded successfully")

except (FileNotFoundError):
  logging.error("sample.ndJson file does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Samples (sample.ndJson) file not found. Sample data are required to run this model. Execution halted.")
  sys.exit(1)
  
# Log number of samples provided by Warehouse to model
model_log_html("Warehouse data provided to model", "h4")
model_log_html("Samples: " + str(len(samples)))

# Process samples
for sample in samples:

  # Add keys representing keys that may or may not already exist
  needed_keys_with_defaults = {
    'id':None,
    'sub_administrative_area_id':None,
    'season_year':None,
    'species':None,
    'age_group':None,
    'sex':None,
    "date_harvested":None,
    'result':None,
    'geolocation_precision':None,
    'latitude':None,
    'longitude':None
    }
  add_missing_defaults_inplace(sample, needed_keys_with_defaults)

  # id
  try:
    sample['id'] = sample['_id']
  except (KeyError, TypeError):
    pass # leave it as none

  # Set sub_administrative_area_id
  try:
    sample["sub_administrative_area_id"] = sample['_sub_administrative_area']['_id']
  except (KeyError, TypeError):
    pass
  
  # Set date_harvested
  if "date_harvested" in sample and isinstance(sample["date_harvested"], str):
    try:
      sample["date_harvested"] = datetime.datetime.strptime(sample['date_harvested'], '%Y-%m-%dT%H:%M:%S.%fZ').strftime('%Y-%m-%d')
    except ValueError: # Input does not match the specified format
      pass 
    type(None).__str__

  # Set result
  if 'tests' in sample and len(sample['tests']) > 0:
    # Create a list of tests that are flagged as selected_definitive (should have length 0 or 1)
    
    tests_selected_definitive = [test for test in sample["tests"] if "selected_definitive" in test and test["selected_definitive"] == True]
    if len(tests_selected_definitive) == 1: # one match so use that test
      # result is a required field; but check if has value. If so, use it, else set to None
      try:
        sample["result"] = tests_selected_definitive[0]["result"] 
      except (KeyError, TypeError, ValueError): 
        pass
    else: 
      sample["result"] = None
  else:
    sample["result"] = None

  # Set geolocation_precision, latitude, and longitude
  if "lat_lng" in sample:
    try: # Using try b/c nested keys
      sample['geolocation_precision'] = sample["lat_lng"]['properties']['geolocation_precision'] 
    except (KeyError, TypeError): 
      sample['geolocation_precision'] = None
    try: # Using try b/c nested keys
      sample["longitude"] = sample["lat_lng"]["geometry"]["coordinates"][0]
      sample["latitude"] = sample["lat_lng"]["geometry"]["coordinates"][1]      
    except (KeyError, TypeError): 
      sample["longitude"] = None
      sample["latitude"] = None

# Write to a CSV
with open(pathlib.Path(base_path / "sample.csv"), 'w', newline='') as f:
  writer = csv.DictWriter(
    f, 
    quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=["id", 
                "sub_administrative_area_id",
                "season_year",
                "species", 
                "age_group",
                "sex", 
                "date_harvested",
                "result", 
                "geolocation_precision",
                "latitude",
                "longitude"
                ], 
    extrasaction='ignore')
  writer.writeheader()
  writer.writerows(samples)

  logging.info("Model input processing successfully completed.")
  sys.exit(0)