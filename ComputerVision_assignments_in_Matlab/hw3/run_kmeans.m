%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: HW 3
%
% Description: Implementation of K-Means clustering
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;

I = imread('PeppersRGB.tif');
img = im2double(I);
[h w d] = size(img);

features = create_feature_vector(img, 'rgb');
%features = create_feature_vector(img, 'lst');
%features = create_feature_vector(img, '');

labeled_features=kmeans(features,3,1);

labeled_image = reshape(labeled_features(:,end), h, w);

out_image = label2rgb(labeled_image, @spring, 'c', 'shuffle');
imshow(out_image);

