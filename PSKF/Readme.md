<b>Paige Saunders Kalman filter for use with ATLAS data</b></br>
Original code (Matlab) by Sivan Toledo 2017</br>
Converted to R by Ingo Schiffner 2017</br>

Contains:</br>
<ul>
<li>InvChol.R - Inverse Cholesky decomposition (required for PSKF)
<li>PSKF.R - Main function for the Paige Saunders Kalman filter
<li>PSKFForATLAS.R - Wrapper function for the Paige Saunders Kalman filter
<li>PSKF_Example.R - Simple implementation example
<li>PSKF_SQLite_Example.R - Implementation using Sqlite data and the Wrapper function
<li>PSKF_SQLite_RW_Example.R - Implementation using Sqlite data and the Wrapper function with a Running Window 
<li>example_locs.txt - Example data used in PSKF_Example
<li>*.sqlite - Example tracks for PSKF_SQLite_Example
</ul>
