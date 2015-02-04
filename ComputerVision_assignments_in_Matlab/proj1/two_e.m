%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 e)
%
% Description: Runs the following task
%
%   e) Process the image using the Sobel edge detector and display the 
%      result.
% 
% References:
% http://www.aquaphoenix.com/lecture/matlab10/page3.html
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{%}
% uncomment this if you want to run this script independently
% (from other parts of Part 2)
clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);
image = im2double(image);


%K = edge(image,'sobel');
sobel_x = [-1 0 1; -2 0 2; -1 0 1];
sobel_y = [1 2 1; 0 0 0; -1 -2 -1];

K = filter2(sobel_x, image);
K = filter2(sobel_y, image);

figure('Name', 'Result with Sobel edge detector','NumberTitle','off');
imshow(K);
