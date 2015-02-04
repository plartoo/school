%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 d)
%
% Description: Runs the following task
%
%   d) Process the image using a 3x3 Median Filter (medfilt2 command) 
%      and display the result.  Comment on the frequency response of 
%      the median filter.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{%}
% uncomment this if you want to run this script independently
% (from other parts of Part 2)
clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);
image = im2double(image);
img_flag = 0;


% apply the median filter
K = medfilt2(image);

if img_flag == 1
    figure('Name', 'Image processed with 3x3 median filter','NumberTitle','off');
    imshow(K);
end

m = K/(image+0.0001);
freqz2(m);

% get the frequency response
%{
[H, f1, f2] = freqz2(K,[1000 1000]);

if img_flag == 1
    figure('Name', 'Frequency response of 3x3 median filter','NumberTitle','off');
    imshow(abs(H), [0 1000]);
end
%}