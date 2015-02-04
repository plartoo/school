%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 3 a)
%
% Description: Runs the following task(s)
%
%   a) Compute the 2D FFT using the fft2 command
%
%   b) Display the magnitude of the 2D-FFT.  Where is the origin
%      located?
%
%   c) Use the ffshift command and then display the magnitude of
%      the 2-D FFT again.  What is the effect of this command on
%      the location of the origin?
%
%   d) Repeat steps b) and c), but this time take the log of the
%      FFT magnitude (plus a small offset) before display.
%      Which method of display do you prefer (with or without log)
%      and why?
%
%   References:
%
%   http://matlabgeeks.com/tips-tutorials/how-to-do-a-2-d-fourier-transform-in-matlab/
%   http://www.mathworks.com/help/toolbox/images/f21-17064.html
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% uncomment this if you want to run this script independently
% (from other parts of Part 2)
clear all;

image = imread('cheshire_normal.JPG');
image = rgb2gray(image);
image = im2double(image);

F = fft2(image);

image_flag = 0;
if image_flag == 1
    figure, imshow(abs(F),[0 1000]), colormap gray;
    title('FFT2 Magnitude for Cheshire normal image');
    figure, imshow(abs(fftshift(F)),[0 1000]), colormap gray;
    title('FFT2 Magnitude shifted to center for Cheshire normal image');
    figure, imshow(angle(fftshift(F)),[-pi pi]), colormap gray;
    title('FFT2 Phase shifted to center for Cheshire normal image');
end

F_log = log(abs(F));

image_flag = 1;
if image_flag == 1
    figure, imshow(F_log,[0 10]), colormap gray;
    title('Log FFT2 Magnitude for Cheshire normal image');
    figure, imshow(fftshift(F_log),[0 10]), colormap gray;
    title('Log FFT2 Magnitude shifted to center for Cheshire normal image');
end

%{
figure('Name', 'Result from adding salt and pepper noise','NumberTitle','off');
imshow(image);
%}


