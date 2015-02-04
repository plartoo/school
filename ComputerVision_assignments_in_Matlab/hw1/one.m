close all;

%% Step 1 and 2: image resizing (to 10x smaller than original)
rgbimage_1 = imread('cheshire_normal.JPG');
resized_rgbimg_1 = imresize(rgbimage_1, [300, 400]);
%figure('Name', 'RGB - Cheshire Under Normal Lighting'); imshow(resized_rgbimg_1);

rgbimage_2 = imread('cheshire_flash.JPG');
resized_rgbimg_2 = imresize(rgbimage_2, [300, 400]);
%figure('Name', 'RGB - Cheshire Under Flash'); imshow(resized_rgbimg_2);

%% turning rgb into gray scale
grayimage_1 = rgb2gray(resized_rgbimg_1);
grayimage_2 = rgb2gray(resized_rgbimg_2);
%figure('Name', 'Grayscale - Cheshire Under Normal Lighting'); imshow(grayimage_1);
%figure('Name', 'Grayscale - Cheshire Under Flash'), imshow(grayimage_2);

%% Step 3: save the grayscale images
imwrite(grayimage_1, 'grayscale_cheshire_normal', 'jpg');
imwrite(grayimage_2, 'grayscale_cheshire_flash', 'jpg');

%% Step 4: zoom to cheshire's location in the grayscale images
cheshire_1 = grayimage_1(35:175, 20:150);
cheshire_2 = grayimage_2(35:175, 20:150);
figure('Name', 'Magnified Grayscale - Cheshire Under Normal Lighting');
imagesc(1:400,1:400,cheshire_1); colormap(gray);
%imagesc(1:400,1:400,cheshire_2); colormap(gray);

% OR we can do this to zoom, too
%imshow(cheshire_1);
%axis([50 100 50 100]);

%% Step 5: save the magnified grayscale images
imwrite(cheshire_1, 'magnified_grayscale_cheshire_normal', 'jpg');
imwrite(cheshire_2, 'magnified_grayscale_cheshire_flash', 'jpg');


%% Step 6: to interpret the intensity values of the above image
%% patch, we invoke external function found in Matlab
%% Help Documentation
my_pixinfotool(cheshire_1);
hold on;
my_pixinfotool(cheshire_2);



%{

%}
