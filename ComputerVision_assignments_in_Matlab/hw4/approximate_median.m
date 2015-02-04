    clear all, close all;
    read_video;

    bg = source(1).cdata;               % read in 1st frame as background frame
    bg_bw = double(rgb2gray(bg));       % convert background to greyscale

    %% set variables
    width = size(bg,2);
    height = size(bg,1);
    fg = zeros(height, width);
    fig1 = figure(1);

    p_thresh = 100;% for hope2 50 % 100;
    a_thresh = 100;% for hope 2 80; %80
    areas = [];
    cur_pts = [];
    max_a = 0;
    
    mtstream = RandStream('mt19937ar');
    RandStream.setDefaultStream(mtstream);
    defaultStream = RandStream.getDefaultStream();
    
    %% start state
    % start from frame 22
    % x = 318, y = 150 at frame 23
    % x = 306, y = 149 at frame 28
    % in six frame, v_x = 14/6; v_y = 1/6

    % ball size r=306-293=~13px


    %% background subtraction
    for i = 2:length(source)
    i;
        fr = source(i).cdata; 
        %fr = imnoise(fr, 'gaussian', 0.1);%, 5, 5)
        %fr = imnoise(fr, 'gaussian');%, 5, 5)
        fr_bw = rgb2gray(fr);           % convert frame to grayscale

        fr_diff = abs(double(fr_bw) - double(bg_bw));   % cast operands as double to avoid negative overflow

        for j=1:width
             for k=1:height

                 if ((fr_diff(k,j) > p_thresh))         % if fr_diff > thresh pixel in foreground 
                     fg(k,j) = fr_bw(k,j);
                 else
                     fg(k,j) = 0;
                 end

                 if (fr_bw(k,j) > bg_bw(k,j))          
                     bg_bw(k,j) = bg_bw(k,j) + 1;           
                 elseif (fr_bw(k,j) < bg_bw(k,j))
                     bg_bw(k,j) = bg_bw(k,j) - 1;     
                 end

             end    
        end

        subplot(1,2,2),imshow(fr);

        fg_bw = im2bw(fg);
        fg_bw = bwareaopen(fg_bw, a_thresh);
        filled = imfill(fg_bw, 8, 'holes');             % fill holes

        stat = regionprops(filled, 'basic');
        subplot(1,2,1),imshow(uint8(fg)), hold on;

%         if i > 9
%             break;
%         end
%          if size(cat(1, stat.Centroid), 1) > 1
%              cat(1,stat.Centroid);
%              %break;
%          end

        cur_pts = [cur_pts; cat(1, stat.Centroid)];

        % at t=100, a=50, I get areas = 77.05 and stdv = 84.16
        % at t=100, a=80, I get areas = 64.09 and stdv = 81.10
        a = cat(1,stat.Area); % max area is 282 according to this calc. at t=100, a=80

        areas = [areas, sum(a)];
        
%         start_state = 0;
%         starting = [622.5, 337.5, 18.0, 45.0];
%         sig_di = 1;     % not exactly sure what the best value is but for now it's set to '1'
%         sig_mi = 5;     % because the noise we added was N(5,5);
%         mi = 1;
% 
%         for i = 1:size(original_bb,1)
%             y_i = original_bb(i);
% 
%             if start_state == 0
%                 x_m = starting(i);
%                 sig_mi = 5;
%                 start_state = 1;    % just one time
%             else
%                 x_m = d_i * x_p;    % (x_i)^- = d_i * (x_i-1)^+
%                 sig_mi = sqrt(sig_di^2 + (d_i * sig_m)^2);   % covariance prediction
%             end
% 
%             % correction
%             x_p = ((x_m * sig_mi^2) + mi * y_i * (sig_m^2))/(sig_mi^2 + (mi^2 * sig_m^2))
%             sig_p = sqrt((sig_mi^2 * sig_m^2)/(sig_mi^2 + (mi * sig_m^2)))
%             bb(i) = x_p;        
%         end
        
        if ~isempty(cur_pts)
            rp = cat(1,stat.BoundingBox);
            if size(rp,1) > 1   % take the midpoint as truth
                rp = mean(rp);
            end

            rpn = addnoise(rp);
            %rpk = kalman(rpn);
            %subplot(1,2,1), rectangle('Position', rp(1,:), 'EdgeColor', 'r', 'LineWidth', 2), hold on;
            %subplot(1,2,1), rectangle('Position', rpn(1,:), 'EdgeColor', 'g', 'LineWidth', 2), hold on;
            subplot(1,2,1), rectangle('Position', rpn1(1,:), 'EdgeColor', 'y', 'LineWidth', 2), hold on;
        end

        M(i-1)  = getframe(fig1);
    end
    
    movie2avi(M,'test_video/just_kalman_result.avi','fps',30);           % save movie as avi    

