%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Phyo Thiha
% Assignment: HW 3
% 
% Description - Creates feature vector from nxm RGB image (3D) matrix.
%
%   The options available are
%       - 'rgb' - turn the original image matrix into [r1, g1, b1; r2 g2, b2; ....] etc
%       - 'hsv' - turn the original image matrix into [h1, s1, v1; h2 s2, v2; ....] etc
%       - 'lst' - turn the original image matrix into [l1, s1, t1; l2 s2, t2; ....] etc, where
%                   + l = (r+g+b)/3
%                   + s = (r-b)/2
%                   + b = (2*g - r - b)/4
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fv = create_feature_vector(img, option)

    r = img(:,:,1);
    g = img(:,:,2);
    b = img(:,:,3);

    if strcmp(option,'rgb') == 1
        fv = [r(:), g(:), b(:)];

    elseif strcmp(option,'lst') == 1
        l = (r(:)+g(:)+b(:))/3;
        s = (r(:)-b(:))/2;
        t = (2*g(:)-r(:)-b(:))/4;
        fv = [l,s,t];
        
    else
        hsv = rgb2hsv(img);
        h = hsv(:,:,1);
        s = hsv(:,:,2);
        v = hsv(:,:,3);
        fv = [h(:), s(:), v(:)];
    end

end
