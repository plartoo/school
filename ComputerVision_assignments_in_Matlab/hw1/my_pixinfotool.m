%% This example shows how to use the tools to create a 
%% simple GUI that provides information and pixels and 
%% features in an image.
%%
%% The source can be found in Matlab - Help Documentation
%% under heading named, "Example: Building a Pixel Infor-
%% mation GUI".
%%

function my_pixinfotool(im)
% Create figure, setting up properties
hfig = figure('Toolbar','none',...
              'Menubar', 'none',...
              'Name','My Pixel Info Tool',...
              'NumberTitle','off',...
              'IntegerHandle','off');

% Create axes and reposition the axes
% to accommodate the Pixel Region tool panel
hax = axes('Units','normalized',...
           'Position',[0 .5 1 .5]);

% Display image in the axes and get a handle to the image
himage = imshow(im);

% Add Distance tool, specifying axes as parent
%hdist = imdistline(hax);

% Add Pixel Information tool, specifying image as parent
hpixinfo = impixelinfo(himage);

% Add Display Range tool, specifying image as parent
hdrange = imdisplayrange(himage);

% Add Pixel Region tool panel, specifying figure as parent
% and image as target
hpixreg = impixelregionpanel(hfig,himage);

% Reposition the Pixel Region tool to fit in the figure
% window, leaving room for the Pixel Information and
% Display Range tools.
set(hpixreg, 'units','normalized','position',[0 .08 1 .4])
