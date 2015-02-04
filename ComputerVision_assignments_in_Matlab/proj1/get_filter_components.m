% Given a separable filter, returns the matrix components
% (horizontal and vertical) of the filter by using singular 
% value decomposition.  The values are turned into POSITIVE.
%
% References:
% http://blogs.mathworks.com/steve/2006/10/04/separable-convolution/
% http://blogs.mathworks.com/steve/2006/11/28/separable-convolution-part-2/
%


function [vertical, horizontal] = get_filter_components(filter)

    [U, S, V] = svd(filter);
    vertical = U(:,1) * sqrt(S(1,1));
    horizontal = V(:,1)' * sqrt(S(1,1));
    
    % set values to positive
    vertical(vertical < 0) = -1 * vertical;
    horizontal(horizontal < 0) = -1 * horizontal;

end
