#!/bin/bash
python3 "scripts/input_processing.py"
exit_status=$?  # Capture exit code
echo "Exit status: $exit_status"

# Check if the first exit code is zero (successful)
if [ $exit_status -eq 0 ]; then  # Use == for comparison
  echo "Input processing ran successfully. Run model script."
  Rscript "scripts/DiseaseClusterAnalysisDataExport.R"
  exit_status=$?  # Capture exit code
  echo "Exit status: $exit_status"
else
  echo "Input processing failed. Exit status: $exit_status"
  exit $exit_status
fi

# Now check the exit code of the R script
if [ $exit_status -eq 0 ]; then  # Consistent comparison
  echo "Model script ran successfully. Run output processing script."
  echo "Exit status: $exit_status"
else
  echo "Model script failed. Exit status: $exit_status"
  exit $exit_status  # Exit with captured exit code
fi

exit $exit_status
