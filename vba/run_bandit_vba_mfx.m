function [p_sub,o_sub,p_group,o_group] = run_bandit_vba_mfx(vba_df)

%Parse out the vba dataframe
y = vba_df.y;
u = vba_df.u;
options = vba_df.options;
dim = options{1}.dim;

%Designate the f and g function handles
f_name = @f_bandit_Qlearn; %Evolution function
g_name = @g_bandit_softmax; %Observation function


%[p_sub,o_sub,p_group,o_group] = VBA_MFX(y,u,f_fname,g_fname,dim,options,priors_group);
[p_sub,o_sub,p_group,o_group] = VBA_MFX(y,u,f_name,g_name,dim,options);


%Pull all the parameters
vba_mfx_df = table();

%Grab the outputs
vba_mfx_df.ID = vba_df.ID;
vba_mfx_df.p_sub = p_sub;
vba_mfx_df.o_sub = o_sub;

for i = 1:length(p_sub)
    vba_mfx_df.alpha_win_vba_mfx(i,1) = p_sub{i,1}.muTheta(1);
    vba_mfx_df.alpha_loss_vba_mfx(i,1) = p_sub{i,1}.muTheta(2);
    vba_mfx_df.decay_vba_mfx(i,1) = p_sub{i,1}.muTheta(3);
    vba_mfx_df.beta_vba_mfx(i,1) = p_sub{i,1}.muPhi(1);
end

save('E:\data\bandit\vba_mfx\behav_vba_mfx_output_n_286_jan_8')