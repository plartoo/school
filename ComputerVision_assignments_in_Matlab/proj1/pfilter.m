%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: Project 1 Part 2 c)
%
% Description: Helper file.
%
% Filter the input image using 3x3 kernel.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function new_image = pfilter(img, kernel)

    kernel_row = size(kernel,1);
    kernel_col = size(kernel,2);

    v_odd = bitand(abs(kernel_row), 1); % if even, this returns zero
    h_odd = bitand(abs(kernel_col), 1);
    if (v_odd == 0) || (h_odd == 0)
        error('Function: filter - Filter dimension must be odd number');
    end
    
    [padded_image, rt, ct] = pad(img, kernel(:,1), kernel(1,:));
    new_image = zeros(size(img));
    
    cols = size(padded_image,2);
    rows = size(padded_image,1);

    for c = 1:cols
        for r = 1:rows
            
            if (r == 1) || (c == 1) || (r == rows) || (c == cols)
                continue;
            end
            
            %{
                padded_image =

                   c-1    c    c+1 
            r-1     0     0     0     0     0
            r       0     1     1     1     0
            r+1     0     1     1     1     0
                    0     1     1     1     0
                    0     0     0     0     0
            %}
            nw = padded_image(r-1,c-1);
            w = padded_image(r,c-1);
            sw = padded_image(r+1,c-1);
            s = padded_image(r+1,c);
            se = padded_image(r+1,c+1);
            e = padded_image(r,c+1);
            ne = padded_image(r-1,c+1);
            n = padded_image(r-1,c);
            ctr = padded_image(r,c);
            
            base_matrix = [nw n ne; w ctr e; sw s se];
            new_image(r-1,c-1) = sum(dot(base_matrix, kernel));

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
