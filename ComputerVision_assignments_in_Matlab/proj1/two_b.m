%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 b)
%
% Description: Runs the following tasks
%
%   b) Process the image using a 3x3 Gaussian Low Pass FIR 
%      (finite impulse response) filter and display the result.
%
%      Use the fspecial command to obtain the filter kernel and the
%      filter2 command to do the filtering.  Obtain the frequency 
%      response of the filter using the freqz2 command.
%
%      Is this filter separable?  If yes, determine its 1-D components 
%      and apply them along the rows and columns respectively.  
%      How does this result compare with the result of filter2?
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
%}
% NOTE: uncomment this if you want to run this
% script independently (from other parts of Part 2)
clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);
img_flag = 1;


% Ref:
% http://amath.colorado.edu/courses/5720/2000Spr/Labs/Worksheets/Matlab_tutorial/matlabimpr.html
image = im2double(image);


% Obtain the filter kernel and display the resulting image
gaussian = fspecial('gaussian');
Y = filter2(gaussian, image);

if img_flag == 1
    subplot(2,2,1);
    imshow(image);
    title('Original image');

    subplot(2,2,2);
    imshow(Y);
    title('Filtered with Guassian');
end

% Display the frequency response of the filter
if img_flag == 1
    subplot(2,2,4);
    title('Frequency response of Gaussian low pass filter');
    freqz2(gaussian);
end


% Determine if the filter is separable
% by checking if its rank is '1' or not
% Ref:
% <http://blogs.mathworks.com/steve/2006/10/04/separable-convolution/>
% <http://blogs.mathworks.com/steve/2006/11/28/separable-convolution-part-2/>
r = rank(gaussian);

if (r == 1)
    [v, h] = get_filter_components(gaussian);
    new_image = filter2(h, image);
    new_image = filter2(v, new_image);
end

% convert from double to uint8
%new_image = im2uint8(new_image);
if img_flag == 1
    subplot(2,2,3);
    imshow(new_image);
    title('Filtered with 1D components of Gaussian');
end

