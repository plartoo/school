% Reference:
% http://eetimes.com/design/military-aerospace-design/4017685/Background-su
% btraction-part-1-MATLAB-models

clear all

read_video;

thresh = 25;           

bg = source(1).cdata;           % read in 1st frame as background frame
bg_bw = rgb2gray(bg);           % convert background to greyscale


% ----------------------- set frame size variables -----------------------
fr_size = size(bg);             
width = fr_size(2);
height = fr_size(1);
fg = zeros(height, width);

% --------------------- process frames -----------------------------------

for i = 2:length(source)
    
    fr = source(i).cdata;       % read in frame
    fr_bw = rgb2gray(fr);       % convert frame to grayscale
    
    fr_diff = abs(double(fr_bw) - double(bg_bw));  % cast operands as double to avoid negative overflow
    
    for j=1:width                 % if fr_diff > thresh pixel in foreground
        for k=1:height
            if ((fr_diff(k,j) > thresh))
                fg(k,j) = fr_bw(k,j);
            else
                fg(k,j) = 0;
            end
        end
    end
    
    bg_bw = fr_bw;
    
    figure(1),subplot(3,1,1),imshow(fr)
    subplot(3,1,2),imshow(fr_bw)
    subplot(3,1,3),imshow(uint8(fg))
    
    M(i-1)  = im2frame(uint8(fg),gray);           % put frames into movie

end

% movie2avi(M,'frame_difference_output', 'fps', 30);           % save movie as avi 
 
    