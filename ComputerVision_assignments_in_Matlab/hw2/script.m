%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment 2, CSC 449
%
% script.m - this script runs different components (i.e.
% matlab files/functions) for assignment #2.
% 
% The name of the function calls should self-explanatory.
% 
% To run, please type below at the terminal -
% $ matlab -r script.m
% OR open this file in Matlab editor and hit F5
%
% Note: you must run the call in the script in order
% since each of them is dependent on the preceding calls.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
img_file = 'shape.png';
threshold = 70;

[img_map, binary_map] = segment(img_file, threshold);
imshow(img_map);

% label connected components
[c_n, l_m, e] = label_cc(binary_map);

% figures out which label to drop based on criteria
[data, drp] = process_labels(l_m, c_n); 

% draw the labels
draw_labels(l_m, data); 

% remove the labels that do not meet the criteria
[new_map, new_data] = clean_labels(l_m, data, drp);

% draw the labels again
draw_labels(new_map, new_data); 
