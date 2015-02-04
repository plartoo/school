%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2
%
% Description:  Script to call the different parts of 
%               Project 1's Part 2 as follows:
%
%   a) Read and display an image.
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
%   c) Process the image using a 3x3 High Pass FIR filter and 
%      display the result.
%
%      This time write your own code that does the filtering.  
%      Explain how you process pixels at the image border.  
%      Display the frequency response of the filter.
%
%   d) Process the image using a 3x3 Median Filter (medfilt2 command) 
%      and display the result.  Comment on the frequency response of 
%      the median filter.
%
%   e) Process the image using the Sobel edge detector and display the 
%      result.
%
%   f) Add Gaussian noise to the image using the imnoise command 
%      and repeat steps b)-e).
%
%   g) Add impulse noise to the image using the imnoise command 
%      and repeat steps b)-e).
%
%   h) Comment on the results obtained in steps b) through g).
%      How does repeated application of the Low Pass filter in b) 
%      affect the results?
%      Is the Sobel edge detector immune to noise?
%      Propose a method for reducing the effects of noise when 
%      High Pass filtering or detecting edges.
%
% WARNING: 
%
%       Since this script runs ALL of Part 2, which include some of 
%       the most computationally intensive image processing, I would
%       strongly recommend against running this script.
%
% Note: 
%   1. Set 'img_flag' to 1 or 0 to display resulting images 
%      on or off in every step.
%
%   2. The scripts/functions must be called in order or else
%      they may not work correctly.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc;
clear all;

% read and display the gray scale image
img_flag = 1;
two_a;

% filtering with Gaussian low pass filter and 1D decomposition of it
img_flag = 1;
two_b;

img_flag = 1;
two_c;

img_flag = 1;
two_d;

img_flag = 1;
two_e;

img_flag = 1;
two_f;

img_flag = 1;
two_g;

