%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 g)
%
% Description: Runs the following task
%
%   g) Add impulse noise to the image using the imnoise command 
%      and repeat steps b)-e).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% uncomment this if you want to run this script independently
% (from other parts of Part 2)
clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);
image = im2double(image);
img_flag = 1;
%{%}

image = imnoise(image,'salt & pepper',0.02);
if img_flag == 1
    figure('Name', 'Result from adding salt and pepper noise','NumberTitle','off');
    imshow(image);
end

two_b;

%{
if img_flag == 1
    figure; imshow(Y); title('Filtered with Guassian');
    figure; imshow(new_image); title('Filtered with 1D components of Gaussian');
    figure; imshow(abs(H), [0 1000]); title('Frequency response of Gaussian low pass filter');
end
%}

two_c;

two_d;

two_e;
