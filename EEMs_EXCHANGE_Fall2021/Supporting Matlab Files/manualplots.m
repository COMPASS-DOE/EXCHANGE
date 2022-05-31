eem=Xs; % Change this. DS is the name of your dataset, eem is used to plot

%% All the options here
figureheight=20; % in centimeter
figurewidth=25; % in centimeter
n=1; % sample counter
r=nrows; % number of rows (subplot)
c=ncols; % number of columns (subplot)
cnt=0; % figure counter.
cntt=ceil(eem.nSample./(r*c)); % Total number of needed figures
ncontours=10; % Number of contour lines
ncolours=10; % Number of color contours
xlimit=[min(eem.Ex) max(eem.Ex)]; % Change this if you want to
ylimit=[min(eem.Em) max(eem.Em)]; % Change this if you want to
clim=[clim_min clim_max]; % Color limit. Change this how you like, e.g. max(eem.X(:))
manualcscale=manualscale; % Do you want to use clim to scale the EEMs?
saveeachfigure=autosave; % Do you want to save all the plots?
figuretitle=figuretitle
%% Set the figure up
fig=dreemfig; % Make a pretty figure with white background
fig.Units="centimeters";
Cpos=get(fig,'pos');
set(fig,'pos', [Cpos(1) Cpos(2) figurewidth figureheight]);
movegui(fig,'center');
clearvars Cpos
fig.Visible='off';drawnow

%% Draw all the plots
while n<=eem.nSample
    i=1;
    t=tiledlayout(r,c,'TileSpacing','tight','Padding','loose')
  while i<=r*c
       if n>eem.nSample
           break
       end
       nexttile(t)
       contourf(eem.Ex,eem.Em,squeeze(eem.X(n,:,:)),ncolours,'LineStyle','none')
       
       hold on
       if manualcscale
           contour(eem.Ex,eem.Em,squeeze(eem.X(n,:,:)),linspace(clim(1),clim(1),ncontours),'Color','k')
       else
           contour(eem.Ex,eem.Em,squeeze(eem.X(n,:,:)),ncontours,'Color','k')
       end
       if manualcscale
          caxis(clim)
       else
           cb=colorbar;
           set(cb,'FontSize',16)
       end
       xlim(xlimit)
       ylim(ylimit)
       if figuretitle
       title(eem.sample_description{n})
       end
       i=i+1;
       n=n+1;
   end
   if manualcscale
       cb=colorbar;
       cb.Layout.Tile = 'east';
       set(cb,'FontSize',16)
   end
   drawnow
   cnt=cnt+1;
   ylabel(t,'Emission (nm)','FontSize',16)
   xlabel(t,'Excitation (nm)','FontSize',16)
   set(gca,'fontsize',16)
   filename=char(sampleID(cnt));
   if saveeachfigure
	  exportgraphics(gcf,[filename,'_plot.png'])
   end
   title(t,['plot ',num2str(cnt),' of ',num2str(cntt)])
   fig.Visible='on';
   if saveeachfigure
   
   else
       pause
   end
   fig.Visible='off';drawnow
end