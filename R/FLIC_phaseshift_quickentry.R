toiso_dfm6_ai <- AI_index_prep(bin30.dfm6.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm6_wells)
toiso_dfm7_ai <- AI_index_prep(bin30.dfm7.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm7_wells)
toiso_dfm8_ai <- AI_index_prep(bin30.dfm8.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm8_wells)
toiso_dfm9_ai <- AI_index_prep(bin30.dfm9.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm9_wells)
toiso_dfm10_ai <- AI_index_prep(bin30.dfm10.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm10_wells)
toiso_dfm11_ai <- AI_index_prep(bin30.dfm11.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm11_wells)
toiso_dfm12_ai <- AI_index_prep(bin30.dfm12.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm12_wells)
toiso_dfm13_ai <- AI_index_prep(bin30.dfm13.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm13_wells)
toiso_dfm14_ai <- AI_index_prep(bin30.dfm14.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm14_wells)
toiso_dfm15_ai <- AI_index_prep(bin30.dfm15.190618.at, idate, itime, etimeS, 7, 8, toiso_dfm15_wells)

toiso_ai <- AI_index(etimeS, etimeE, 'toiso', 
                      toiso_dfm6_ai, 
                      toiso_dfm7_ai, 
                      toiso_dfm8_ai, 
                      toiso_dfm9_ai, 
                      toiso_dfm10_ai, 
                      toiso_dfm11_ai, 
                      toiso_dfm12_ai, 
                      toiso_dfm13_ai, 
                      toiso_dfm14_ai, 
                      toiso_dfm15_ai)

toiso_dfm6_d7_8 <- subset.data(bin30.dfm6.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm6_wells)
toiso_dfm7_d7_8 <- subset.data(bin30.dfm7.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm7_wells)
toiso_dfm8_d7_8 <- subset.data(bin30.dfm8.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm8_wells)
toiso_dfm9_d7_8 <- subset.data(bin30.dfm9.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm9_wells)
toiso_dfm10_d7_8 <- subset.data(bin30.dfm10.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm10_wells)
toiso_dfm11_d7_8 <- subset.data(bin30.dfm11.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm11_wells)
toiso_dfm12_d7_8 <- subset.data(bin30.dfm12.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm12_wells)
toiso_dfm13_d7_8 <- subset.data(bin30.dfm13.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm13_wells)
toiso_dfm14_d7_8 <- subset.data(bin30.dfm14.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm14_wells)
toiso_dfm15_d7_8 <- subset.data(bin30.dfm15.190618.at, idate, itime, 0800, 7, 8, 'nonnorm', 'running', toiso_dfm15_wells)



toiso_all_d7_8 <- combine_days(1, 
                               # toiso_dfm6_d7_8, 
                                toiso_dfm7_d7_8, 
                                toiso_dfm8_d7_8, 
                                toiso_dfm9_d7_8, 
                                toiso_dfm10_d7_8, 
                                toiso_dfm11_d7_8, 
                                toiso_dfm12_d7_8, 
                                #toiso_dfm13_d7_8, 
                                toiso_dfm14_d7_8, 
                                toiso_dfm15_d7_8)

toiso_dfm6_aiPS <- AI_phase_score(bin30.dfm6.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm6_wells)
toiso_dfm7_aiPS <- AI_phase_score(bin30.dfm7.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm7_wells)
toiso_dfm8_aiPS <- AI_phase_score(bin30.dfm8.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm8_wells)
toiso_dfm9_aiPS <- AI_phase_score(bin30.dfm9.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm9_wells)
toiso_dfm10_aiPS <- AI_phase_score(bin30.dfm10.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm10_wells)
toiso_dfm11_aiPS <- AI_phase_score(bin30.dfm11.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm11_wells)
toiso_dfm12_aiPS <- AI_phase_score(bin30.dfm12.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm12_wells)
toiso_dfm13_aiPS <- AI_phase_score(bin30.dfm13.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm13_wells)
toiso_dfm14_aiPS <- AI_phase_score(bin30.dfm14.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm14_wells)
toiso_dfm15_aiPS <- AI_phase_score(bin30.dfm15.190618.at, 'toiso', idate, itime, etimeS, etimeE, 7, 8, toiso_dfm15_wells)

toiso_aiPS_all <- bind_rows(toiso_dfm6_aiPS, 
                             toiso_dfm7_aiPS, 
                             toiso_dfm8_aiPS, 
                             toiso_dfm9_aiPS,
                             toiso_dfm10_aiPS,
                             toiso_dfm11_aiPS,
                             toiso_dfm12_aiPS,
                             toiso_dfm13_aiPS,
                             toiso_dfm14_aiPS,
                             toiso_dfm15_aiPS)


toiso_dfm6_PS <- phaseshift_genotype_time(bin30.dfm6.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm6_wells)
toiso_dfm7_PS <- phaseshift_genotype_time(bin30.dfm7.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm7_wells)
toiso_dfm8_PS <- phaseshift_genotype_time(bin30.dfm8.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm8_wells)
toiso_dfm9_PS <- phaseshift_genotype_time(bin30.dfm9.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm9_wells)
toiso_dfm10_PS <- phaseshift_genotype_time(bin30.dfm10.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm10_wells)
toiso_dfm11_PS <- phaseshift_genotype_time(bin30.dfm11.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm11_wells)
toiso_dfm12_PS <- phaseshift_genotype_time(bin30.dfm12.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm12_wells)
toiso_dfm13_PS <- phaseshift_genotype_time(bin30.dfm13.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm13_wells)
toiso_dfm14_PS <- phaseshift_genotype_time(bin30.dfm14.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm14_wells)
toiso_dfm15_PS <- phaseshift_genotype_time(bin30.dfm15.190618.at, 'toiso', idate, itime, etimeS, etimeE, pday, fday, 'norm', toiso_dfm15_wells)

toiso_PS_all <- bind_rows(toiso_dfm6_PS, 
                          toiso_dfm7_PS,
                          toiso_dfm8_PS,
                          toiso_dfm9_PS, 
                          toiso_dfm10_PS,
                          toiso_dfm11_PS,
                          toiso_dfm12_PS,
                          toiso_dfm13_PS,
                          toiso_dfm14_PS,
                          toiso_dfm15_PS
                          )

