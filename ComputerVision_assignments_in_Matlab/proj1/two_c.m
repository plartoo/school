%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 c)
%
% Description: Runs the following task
%
%   c) Process the image using a 3x3 High Pass FIR filter and 
%      display the result.
%
%      This time write your own code that does the filtering.  
%      Explain how you process pixels at the image border.  
%      Display the frequency response of the filter.
% 
% References:
% http://idlastro.gsfc.nasa.gov/idl_html_help/Filtering_an_Imagea.html
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
img_flag = 1;


% kernel for high pass filter
% [ -1  -1  -1
%   -1   8  -1
%   -1  -1  -1 ]
kernel = ones(3, 3) * -1;
kernel(2,2) = 8;

% apply the high pass filter
new_image = pfilter(image, kernel);

if img_flag == 1
    K=mat2gray(new_image);
    figure('Name', 'Image processed with 3x3 high pass filter','NumberTitle','off');
    title('Image processed with 3x3 high pass filter');
    imshow(K);
end


% get the frequency response
if img_flag == 1
    figure('Name', 'Frequency response of 3x3 high pass filter','NumberTitle','off');
    freqz2(kernel);
end

%{
% this works, too and much faster
% generate 3x3 kernel for high pass filter
kernel = ones(3, 3) * -1;
kernel(2,2) = 9;
J = conv2(image, kernel, 'same'); % Convolve keeping size of I
K=mat2gray(J);
imshow(K);
colormap(gray);
%}

