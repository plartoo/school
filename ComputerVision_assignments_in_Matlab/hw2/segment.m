%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% segment.m - this function reads the image,
% builds a histogram, creates binary map
% and output a matrix modified from the binary
% map.
%
%
% Usage: In Matlab script, call this function as below:
%
%   >>[img_map, binary_map] = segment(img_file)
%
% where 'img_map' is a 2D array whose entries are
% either '0' (representing background) or '255'
% (representing foreground).
% 'binary_map' is a 2D array whose entries are either '0'
% (representing background) or '1' (representing foreground).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [img_map, binary_map] = segment(img_file, threshold)

I = imread(img_file);

%% Create a histogram out of the image pixel values
counts = histc(I(:),0:1:255);
%bar(counts,10); % will comment this out for now
%set(gca,'XLim',[1 length(counts)]);

%% Based on histogram we tried '50', '70' and '160' 
% as threshold values
logical1 = (I < threshold); % use logical array indexing
logical2 = (I >= threshold);

%% Create binary map
binary_map = I;
binary_map(logical1) = 0;
binary_map(logical2) = 1;

%% Assign all the object pixels a value of
%% '255' and the background value of '0'
img_map = binary_map * 255;







%{
% ignore this because we are not allowed to use
% built-in Matlab functions for this assignment
% get global image threshold
level = graythresh(I); % yields 0.3059

% convert image to binary
I = im2bw(I, level);

% assign all object pixels to '255' and the rest '0'
I1 = I*255;
%imshow(I);

L = bwlabel(I1; 8);

%}