%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 f) or g)
%
% Description: 
% 
%   Script to run either part 2 f) or g) in additive fashion.
% That is, we load the image only once and additively apply 
% different filters (gaussian, median- and high-pass) to the
% the image.   This is, therefore, different from individually 
% running the scripts like "two_b.m", "two_c.m" and so on, which 
% reload the image.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;

image = imread('cheshire_flash_small.JPG');
image = rgb2gray(image);
image = im2double(image);

figure('Name', 'Original image','NumberTitle','off');
imshow(image);

%i1 = imnoise(image,'gaussian');    % for part 2 f)
i1 = imnoise(image,'salt & pepper',0.02);   % for part 2 g)
figure('Name', 'Result from adding Gaussian noise','NumberTitle','off');
imshow(i1);

% 3d gaussian
gaussian = fspecial('gaussian');
i2 = filter2(gaussian, i1);
figure('Name', 'after 3D guassian filtered','NumberTitle','off');
imshow(i2);

% 3x3 high pass
kernel = ones(3, 3) * -1;
kernel(2,2) = 8;
i5 = pfilter(i2, kernel);
i5_image =mat2gray(i5);
figure('Name', 'after 3x3 high pass filter','NumberTitle','off');
imshow(i5);% colormap(gray);

% 3x3 median pass
i6 = medfilt2(i5);
figure('Name', 'afte median filtered','NumberTitle','off');
imshow(i6);
%imshow(mat2gray(i6)); colormap gray;

sobel_x = [-1 0 1; -2 0 2; -1 0 1];
sobel_y = [1 2 1; 0 0 0; -1 -2 -1];
i7 = filter2(sobel_x, image);
i7 = filter2(sobel_y, image);
figure('Name', 'Result with Sobel edge detector','NumberTitle','off');
imshow(i7);
