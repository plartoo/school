%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% clean_labels.m - this function drops the labels from the
% labeled_map and the data cell, both produced by 'label_cc.m'
% and 'process_labels.m', respectively.
%
% It expects 
%   'orig_map'           - labeled map of an image
%   'data'          - Matlab cell that contains area_value, 
%                     bounding_box and center_points for
%                     each label.
%   'drp'           - array of labels to be dropped
%
%
% Usage: In Matlab script, call this function as below:
%
%   >> [newly_labeled_map, new_data] = clean_labels(orig_map, data, drp)
%
% Return values are a newly labeled map and data, whose 
% pertinent labels/elements are dropped/deleted.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [newly_labeled_map, new_data] = clean_labels(orig_map, data, drp)

    newly_labeled_map = orig_map;

    for i = 1:numel(drp)

        label = drp(i);
        logical_indx = (orig_map == label);
        newly_labeled_map(logical_indx) = 0;

    end
   

    % drop labels from data
    data(:,drp) = [];
    new_data = data;

end
    