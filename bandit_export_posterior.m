

for injectNoise = [0 0.2 0.4]
num2str(injectNoise);
load(sprintf('/Users/jiazhouchen/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data_unbound/2lr_decay/%s/vba_MFX_output/bandit_vba_MFX_output_2lr_decay.mat',num2str(injectNoise)))
for xj = 1:length(posterior_sub)
    muTheta_1(xj)=posterior_sub{xj}.muTheta(1);
    muTheta_2(xj)=posterior_sub{xj}.muTheta(2);
    muTheta_3(xj)=posterior_sub{xj}.muTheta(3);
    muPhi(xj)=posterior_sub{xj}.muPhi;
    ID(xj)=posterior_sub{xj}.id;
    NOISE(xj)=0;
end
allinfo=table(muTheta_1',muTheta_2',muTheta_3',muPhi',NOISE',ID','VariableNames',{'muTheta_1','muTheta_2','muTheta_3','muPhi','NOISE','ID'});
writetable(allinfo,sprintf('noise%s.csv',num2str(injectNoise)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
datadir=sprintf('~/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data/2lr_decay/%s',num2str(injectNoise));
dirx=dir(datadir);
dirx=dirx(~[dirx.isdir]);
dirx=dirx(~strcmp('.DS_Store',{dirx.name}));

for xk = 1:length(dirx)
    dx = dirx(xk);
    load(fullfile(dx.folder,dx.name))
    values=values(:,1:300);
    posterior=posterior_sub{find(ID==bx.id,1)};
    muXs=posterior.muX';
    writetable(table(double(muXs),decisions',gxs',values',noises',repmat(bx.noise,300,1),repmat(bx.id,300,1),'VariableNames',{'muXs','decision','gxs','values','noises','noise','ID'}),fullfile(datadir,"csvoutput",sprintf('ID_%d_%d.csv',bx.id,xk)),'Delimiter',',')
end

end