function boundingBox = addnoise(original_bb)
    % returns noise added bounding box
    if randi(0:1) > 0
        multiplier = -1;
    else
        multiplier = 1;
    end
    
    noise = randi(10, 1, 4);
    
    noise(1) = noise(1) * multiplier;
    noise(2) = noise(2) * multiplier;
    boundingBox = noise + original_bb;

end
