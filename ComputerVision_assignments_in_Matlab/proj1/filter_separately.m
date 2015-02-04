%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1
%
% Description: Helper file.
%
% Given a two 1D filters (horizontal and vertical) and an image,
% filter the image along the rows using horizontal filter and 
% do the same along the columns using vertical filter.
%
% NOTE: the 1D filters must be of odd-numbered length.
%
% Returns a new, filtered image.
%
% References:
% http://www.cse.unr.edu/~bebis/CS485/Lectures/ImageFiltering.ppt
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function new_image = filter_separately(img, v, h)

    v_odd = bitand(abs(size(v,1)), 1); % if even, this returns zero
    h_odd = bitand(abs(size(h,2)), 1);
    if (v_odd == 0) || (h_odd == 0)
        error('Function: filter_separately - BOTH filters must be of odd numbered length');
    end
    
    [padded_image, rt, ct] = pad(img, v, h);
    new_image = zeros(size(img));

    % row-wise filtering
    c_i = 0;
    md = median([1:size(h,2)]);

    for c=(ct+1):(size(padded_image,2)-ct)
        c_i = c_i + 1;

        for r=(rt+1):(size(padded_image,1)-rt)
            sum = 0;

            for t=-ct:1:ct
                sum = sum + padded_image(r, c+t) * h(1, md+t);
            end
            
            new_image(r-rt, c_i) = sum;
        end
    end

    % copy intermediate results
    [padded_image, rt, ct] = pad(new_image, v, h);

    % column-wise filtering
    r_i = 0;
    md = median([1:size(v,1)]);

    for r=(rt+1):(size(padded_image,1)-rt)
        r_i = r_i + 1;

        for c=(ct+1):(size(padded_image,2)-ct)
            sum = 0;

            for t=-rt:1:rt
                sum = sum + padded_image(r+t, c) * v(md+t, 1);
            end
            
            new_image(r_i, c-ct) = sum;
        end
    end

end



%% pads the boundaries of the original image
%  with zeros on either sides of the boundary.
%  The thickness of the boundary is defined 
%  by floor(v) or floor(h), where 'v' and 'h'
%  are 1D filter arrays for vertical(rows) and
%  horizontal(columns), respectively.
%
%  Returns the padded_image, row_thickness and 
%  column_thickness.
%
function [img, r_thick, c_thick] = pad(orig_img, v, h)

    [br bc] = size(orig_img);
    img = orig_img;

    r_thick = floor(size(v,1)/2);
    row_padding = zeros(r_thick, bc);
    img = [row_padding; img; row_padding];

    c_thick = floor(size(h,2)/2);
    column_padding = zeros(size(img, 1), c_thick);
    img = [column_padding, img, column_padding];
   
end


%{
b = [2 3 3; 3 5 5; 4 4 6];
h = [1 2 1];
h = [1 2 1];
v = [1;2;1];
v = [1;2;1;2;1];
filter_separately(b,v,h)
%}
