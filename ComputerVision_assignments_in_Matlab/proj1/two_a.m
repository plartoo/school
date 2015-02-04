%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 a)
%
% Description: Runs the following tasks
%
%   a) Read and display an image.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);

if img_flag == 1
    figure;
    imshow(image);
end
