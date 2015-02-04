%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% draw_labels.m - this function draws the labels produced
% as a result of 'segment.m', 'label_cc.m' and 
% 'process_labels.m'.
%
% It expects 
%   'map'           - labeled map of an image
%   'data'          - Matlab cell that contains area_value, 
%                     bounding_box and center_points for
%                     each label.
%
%
% Usage: In Matlab script, call this function as below:
%
%   >> out = draw_labels(map, data)
%
% This will draw the labels and their bounding box in two RGB 
% figures.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function out = draw_labels(map, data)

    figure;
    title('RGB image of labeled data (All components shown)') 

    rgb_label_1 = label2rgb(map, @spring, 'c', 'shuffle');
    imshow(rgb_label_1);

    hold on;
    
    for i = 1:size(data,2)
        dim = [data{3,i}(1,:), data{5,i}(1,:)+1]; % [x y w h]
        c_x = data{4,i}(1,1); % center x
        c_y = data{4,i}(1,2); % center y
        rectangle('Position', dim, 'LineWidth',1,'LineStyle','-');

        text(c_x, c_y, num2str(i), 'HorizontalAlignment', 'left', ...
            'VerticalAlignment', 'bottom', 'FontSize', 10);
    end
    
