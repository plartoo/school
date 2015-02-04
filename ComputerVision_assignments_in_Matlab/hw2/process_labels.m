%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process_labels.m - this function collect the labels to be 
% dropped by
%   - removing regions having areas smaller than 10px (rule 1)
%   - removing all skinny regions having aspect ratios
%     of 3:1 (w:h), using rectangular bounding box (rule 2)
%
%
% Usage: In Matlab script, call this function as below:
%
%   >>[data, dropped] = process_labels(labeled_map, component_count)
%
% where 'labeled_map' is a 2D array whose entries are
% labeled by 8-connected componenet algorithm. 'component_count'
% is the number of components/labels that exist in 'labeled_map'.
% 
% 'data' is Matlab cell containing area_value, bounding_box 
% and center_points for each label.  'dropped' is list of labels 
% that did not meet the rules above and get dropped.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [data, dropped] = process_labels(labeled_map, component_count)

    % data = {
    %            [label_1]             [label_2]            [label3]  ....
    %
    %            [area_1]              [area_2]             [area_3]  ....
    %
    %            [ low_x, low_y;       [ low_x, low_y;      [ low_x, low_y;
    %             high_x, high_y]       high_x, high_y]     high_x, high_y]
    %
    %            [center_x, center_y]  [center_x, center_y] [center_x, center_y]
    %
    %            [width, height]       [width, height]      [width, height]
    %
    %        }
    data = {};
    

    %% initialize the cell with labels and area values
    max_int = intmax('uint32');
    for c = 1:component_count
        % I know I could, but I don't want to clean the labels here
        area = size(labeled_map(labeled_map==c),1);
        data = [data, {c; area; [max_int, max_int; 0, 0]; [0, 0]}];
    end

    
    %% calculate bounding box coordinates
    rows = size(labeled_map,1);
    cols = size(labeled_map,2);
    
    for c = 1:cols

        for r = 1:rows
            
            cur_label = labeled_map(r, c);

            if (cur_label ~= 0)
                data{3, cur_label}(1,1) = min(data{3, cur_label}(1,1), c); % low_x
                data{3, cur_label}(1,2) = min(data{3, cur_label}(1,2), r); % low_y
                
                data{3, cur_label}(2,1) = max(data{3, cur_label}(2,1), c); % high_x
                data{3, cur_label}(2,2) = max(data{3, cur_label}(2,2), r); % high_y
                                
            end

        end
        
    end
    
    
    %% now collect the labels to drop
    dropped = [];

    for i = 1:size(data,2)

        width = data{3, i}(2,1) - data{3, i}(1,1);
        height = data{3, i}(2,2) - data{3, i}(1,2);

        % find center point for later drawing
        data{4,i}(1,1) = data{3, i}(1,1) + (width/2); % center x
        data{4,i}(1,2) = data{3, i}(1,2) + (height/2); % center y

        data{5,i}(1,1) = width;
        data{5,i}(1,2) = height;

        
        if (data{2,i} < 10) || (width/height > 3)
            dropped = [dropped, i];
        end

    end
    
end


%{
References and URLs:
http://www.mathworks.com/matlabcentral/newsreader/view_thread/162725
http://www.mathworks.com/help/toolbox/images/ref/regionprops.html#bqkf8hf
http://www.mathworks.com/help/techdoc/matlab_prog/br04bw6-98.html#br1zng6-1
http://labelme.csail.mit.edu/LabelMeToolbox/index.html
http://homepages.inf.ed.ac.uk/rbf/HIPR2/label.htm
====

    % drop elimited labels from 'data'
    %data(:,dropped) = [];
    %n_dropped = [data{1,:}]; % labels still remaining

====
c
c = {'1','2','3';
'4','5','6';
'7','8','9';}

c = 

    '1'    '2'    '3'
    '4'    '5'    '6'
    '7'    '8'    '9'

c{1,1} = [1 1]

c = 

    [1x2 double]    '2'    '3'
    '4'             '5'    '6'
    '7'             '8'    '9'

c(:,1)

ans = 

    [1x2 double]
    '4'         
    '7' 


c{:,1}

ans =

     1     1


ans =

4


ans =

7

c = 

    [1x2 double]    '2'    '3'    [3x2 char]
    '4'             '5'    '6'            []
    '7'             '8'    '9'            []


c(:,4) = ['10';'11';'12']
??? Conversion to cell from char is not possible.
 
c(:,4) = []

c = 

    [1x2 double]    '2'    '3'
    '4'             '5'    '6'
    '7'             '8'    '9'

c(:,4) = {'10';'11';'12'}

c = 

    [1x2 double]    '2'    '3'    '10'
    '4'             '5'    '6'    '11'
    '7'             '8'    '9'    '12'

c{1,4,:} = ['10';'11';'12']

c = 

    [1x2 double]    '2'    '3'    [3x2 char]
    '4'             '5'    '6'            []
    '7'             '8'    '9'            []


%}