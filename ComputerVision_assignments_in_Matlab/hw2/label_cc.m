%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_cc.m - labels connected components using
% 8 neighbors. RETURNS total number of components
% and the labeled map.
%
%
% Usage: In Matlab script, call this function as below:
%
%   >>[component_num, labeled_map, e] = label_cc(img_map)
%
% where 'img_map' can be any 2D array whose entries are
% either '0' (representing background) or any number '> 0'
% (representing foreground).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [component_num, labeled_map, e] = label_cc(img_map)

    rows = size(img_map,1);
    cols = size(img_map,2);

    % initialize return variables
    labeled_map = zeros(rows,cols);
    component_num = 0;

    % equivalence table
    e = 1:rows*cols;
    e_table = []; % just for debugging

    % we will visit neighbors of pixel 'p'
    % in the following order: ne, n, nw, w.
    %  _ _  _ _ _ _ _ _ _
    % |     |     |
    % | nw  |  n  |
    % - - - - - - -
    % |     |     |
    % | w   |  p  |
    % - - - - - - -
    % |     |
    % | sw  |
    % - - - -
    %

    %% First pass
    % iterate by columns because Matlab arrange them in memory as such
    % (therefore, our code is run utilizing optimized cache fetching)
    for c = 1:cols 

        for r = 1:rows

            if (r == 1) % boundary condition #1: first row
                n = 0;
                nw = 0;

                if (c == 1) % first column
                    w = 0;
                    sw = 0;
                else
                    w = labeled_map(r, c-1);
                    sw = labeled_map(r+1, c-1);
                end

            elseif (r == rows) % boundary condition #2: last row
                n = labeled_map(r-1, c);
                sw = 0;

                if (c == 1)
                    nw = 0;
                    w = 0;
                else
                    nw = labeled_map(r-1, c-1);
                    w = labeled_map(r, c-1);
                end
                
            else % not the first row nor the last
                n = labeled_map(r-1, c);

                if (c == 1)
                    nw = 0;
                    w = 0;
                    sw = 0;
                else
                    nw = labeled_map(r-1, c-1);
                    w = labeled_map(r, c-1);
                    sw = labeled_map(r+1, c-1);
                end

            end

            neighbors = [n, nw, w, sw];

            if ( img_map(r,c) ~= 0 )

                if (sum(neighbors ~= 0) == 0) % if all neighbors are background pixel
                    component_num = component_num + 1;
                    labeled_map(r,c) = component_num;

                elseif (sum(neighbors ~= 0) == 1) % if just one neighbor is labeled
                    labeled_map(r,c) = neighbors((neighbors ~= 0) == 1); % assign that label

                else % if more than one neighbor is labeled
                    labeled_map(r,c) = min(neighbors( find(neighbors) )); % assign the smallest, NON-ZERO label

                    e = update_e_table(neighbors, e);

                    %{
                    % (debug) create a combination of different labels in neighbors
                    combo_n = combnk(neighbors(neighbors ~= 0), 2);
                    combo_n = unique(combo_n, 'rows'); % find unique pairs
                    combo_n = combo_n( combo_n(:,1) ~= combo_n(:,2), : ); % eliminate self-directing labels such as (1, 1), (2, 2)
                    e_table = [e_table; combo_n];
                    %}

                end
            end

        end

    end

    %{
    % (debug) sort and return unique pairs in equivalence table
    e_table = unique(sort(e_table), 'rows');
    %}

    %% Second pass
    %  Note: I was resolving equivalence table using java Hash, but I
    %  came across this site
    %  <http://people.sc.fsu.edu/~jburkardt/m_src/image_components/i4mat_compon
    %  ents.m>,
    %  which showed me a simpler way to handle it.
    %  Therefore, I borrowed and modified their code to suit my needs.

    %  When a component has multiple labels, have the higher labels
    %  point to the lowest one.
    for component = component_num : -1 : 1
        b = component;

        while ( e(b) ~= b )
            b = e(b);
        end

        e(component) = b;

    end

    % Locate the minimum label for each component.
    % Assign these mininum labels new consecutive indices.
    q = zeros ( 1, component_num );
    i = 0;

    for component = 1 : component_num

        if ( e(component) == component )
            i = i + 1;
            q(component) = i;
        end

    end

    component_num = i;

    % Replace the labels by consecutive labels.
    % Need to avoid c(i,j) = 0.

    i = find ( labeled_map ~= 0 );
    labeled_map(i) = q( e( labeled_map(i) ) );

    return

end

% Replace values in equivalence table, 'e', with equivalent, but lower values
% E.g., given neighbors = [1 2 2 0], we will set e(2) = 1 so that when we 
% do the second pass, we will have an easier time, labeling them in order 
% of minimum to maximum labels
function e = update_e_table(neighbors, e)
    neighbors = neighbors(neighbors~=0); % remove zeros
    neighbors = sort(neighbors, 'descend');
    
    for i = 1:(size(neighbors, 2)-1) % iterate before the minimum
        e(neighbors(i)) = neighbors(end);
    end
end




%{

Reference URLs used in writing this code:

http://blogs.mathworks.com/steve/2007/05/11/connected-component-labeling-part-5/
http://blogs.mathworks.com/steve/2007/03/20/connected-component-labeling-part-3/
http://people.sc.fsu.edu/~jburkardt/m_src/image_components/image_components.html
http://people.sc.fsu.edu/~jburkardt/m_src/image_components/i4mat_components.m
http://www.mathworks.com/company/newsletters/articles/Matrix-Indexing-in-MATLAB/matrix.html
http://www.mathworks.com/help/toolbox/stats/combnk.html
http://stackoverflow.com/questions/3591942/hash-tables-in-matlab
http://www.mathworks.com/help/techdoc/ref/javaarray.html
http://www.mathworks.com/matlabcentral/newsreader/view_thread/159824
http://www.mathworks.com/matlabcentral/answers/27314-how-to-remove-elements-in-a-matrix-less-than-a-threshold
http://www.mathworks.com/help/techdoc/ref/sort.html

%}

%{

%% I tried using Hash, but figured out a simpler way
%% to store equivalence table. This is only kept for
%% future reference


% now turn this into hash; I didn't turn this into
% hash earlier because it seems like a mess to 
% deal with Java Arrays in Matlab
e_hash = hashify(e_table);

function hash = hashify(two_by_n_array)
    hash = java.util.Hashtable;
    
    for i = 1:size(two_by_n_array,1) % for each pair in equivalence_table
        pair = two_by_n_array(i,:); % e.g., [1 2]
        hash.put(pair(1), pair(2));        
    end

end

%}