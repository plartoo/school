%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 1
%
% Description: Runs the following tasks
%
%   a) Use the imread command to read an image.  
%   Images are available at the course web site*. 
%   Use the pair of images taken in homework #1 - 
%   In Step 3, describe the effect of lighting change, linear or nonlinear, 
%   on the Fourier transform (magnitude)?)
%
%   b) Use the commands imshow and imagesc to display an image.  
%   What is the difference between these two commands?
%
%   c) Use the colormap and colorbar commands to adjust the colormap and 
%   display the colorbar.
%
%   d) Use the imwrite command to write the image in a new file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;

image = imread('cheshire_normal_small.JPG');
image = rgb2gray(image);

% instead of rgb2gray above, we can try indexed image, too
%map = colormap(bone);
%image = rgb2ind(image, map);

% use 'imshow'
figure;
imshow(image);

% use 'imagesc'
figure;
imagesc(image);


% we must turn this into indexed image; otherwise, colormap won't work
map = colormap(bone);
imagesc(image);
colorbar;

% use 'imwrite'
imwrite(image, 'new_img.jpg');
