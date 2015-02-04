function bb = kalman(original_bb)
    % returns noise added bounding box

    start_state = 0;
    starting = [622.5, 337.5, 18.0, 45.0];
    sig_di = 1;     % not exactly sure what the best value is but for now it's set to '1'
    sig_mi = 5;     % because the noise we added was N(5,5);
    mi = 1;
    
    for i = 1:size(original_bb,1)
        y_i = original_bb(i);
        
        if start_state == 0
            x_m = starting(i);
            sig_mi = 5;
            start_state = 1;    % just one time
        else
            x_m = d_i * x_p;    % (x_i)^- = d_i * (x_i-1)^+
            sig_mi = sqrt(sig_di^2 + (d_i * sig_m)^2);   % covariance prediction
        end
        
        % correction
        x_p = ((x_m * sig_mi^2) + mi * y_i * (sig_m^2))/(sig_mi^2 + (mi^2 * sig_m^2))
        sig_p = sqrt((sig_mi^2 * sig_m^2)/(sig_mi^2 + (mi * sig_m^2)))
        bb(i) = x_p;        
    end    
    

end
