%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 f)
%
% Description: Runs the following task
%
%   f) Add Gaussian noise to the image using the imnoise command 
%      and repeat steps b)-e).
% 
% References:
% http://www.mathworks.com/help/toolbox/images/ref/imnoise.html
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% uncomment this part if you want to run 
% this script independently (from other parts of Part 2)
clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);
image = im2double(image);
img_flag = 0;
%{%}

image = imnoise(image,'gaussian');
if img_flag == 1
    figure('Name', 'Result from adding Gaussian noise','NumberTitle','off');
    imshow(image);
end

two_b;
%{
if img_flag ==1
    figure; imshow(Y); title('Filtered with Guassian');
    figure; imshow(new_image); title('Filtered with 1D components of Gaussian');
    figure; imshow(abs(H), [0 1000]); title('Frequency response of Gaussian low pass filter');
end
%}

two_c; % can take up to ~3mins for large images

two_d;

two_e;
