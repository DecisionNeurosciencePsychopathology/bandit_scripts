function [tmp_len] = get_reg_rt_length(reg_rt,i,ct)
for j = 1:length(reg_rt)
    if reg_rt(j)==1 && reg_rt(j+1)==0
        ct = ct+1;
        i=i+1;
        tmp_len(i) = ct;
        ct=0;
    elseif reg_rt(j)==1
        ct = ct+1;
    end
end
return

