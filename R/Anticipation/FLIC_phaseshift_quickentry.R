pdfiso_dfm6_ai <- AI_index_prep(bin30.dfm6.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm6_wells)
pdfiso_dfm7_ai <- AI_index_prep(bin30.dfm7.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm7_wells)
pdfiso_dfm8_ai <- AI_index_prep(bin30.dfm8.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm8_wells)
pdfiso_dfm9_ai <- AI_index_prep(bin30.dfm9.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm9_wells)
pdfiso_dfm10_ai <- AI_index_prep(bin30.dfm10.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm10_wells)
pdfiso_dfm11_ai <- AI_index_prep(bin30.dfm11.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm11_wells)
pdfiso_dfm12_ai <- AI_index_prep(bin30.dfm12.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm12_wells)
pdfiso_dfm13_ai <- AI_index_prep(bin30.dfm13.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm13_wells)
pdfiso_dfm14_ai <- AI_index_prep(bin30.dfm14.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm14_wells)
pdfiso_dfm15_ai <- AI_index_prep(bin30.dfm15.190327.at, idate, itime, etimeS, 7, 8, pdfiso_dfm15_wells)

pdfiso_ai <- AI_index(etimeS, etimeE, 'pdfiso', 
                      pdfiso_dfm6_ai, 
                      pdfiso_dfm7_ai, 
                      pdfiso_dfm8_ai, 
                      pdfiso_dfm9_ai, 
                      pdfiso_dfm10_ai, 
                      pdfiso_dfm11_ai, 
                      pdfiso_dfm12_ai, 
                      pdfiso_dfm13_ai, 
                      pdfiso_dfm14_ai, 
                      pdfiso_dfm15_ai)
# for plotting
pdfcyc_dfm6_d2_7 <- subset.data(bin30.dfm6.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm6_wells)
pdfcyc_dfm7_d2_7 <- subset.data(bin30.dfm7.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm7_wells)
pdfcyc_dfm8_d2_7 <- subset.data(bin30.dfm8.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm8_wells)
pdfcyc_dfm9_d2_7 <- subset.data(bin30.dfm9.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm9_wells)
pdfcyc_dfm10_d2_7 <- subset.data(bin30.dfm10.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm10_wells)
pdfcyc_dfm11_d2_7 <- subset.data(bin30.dfm11.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm11_wells)
pdfcyc_dfm12_d2_7 <- subset.data(bin30.dfm12.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm12_wells)
pdfcyc_dfm13_d2_7 <- subset.data(bin30.dfm13.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm13_wells)
pdfcyc_dfm14_d2_7 <- subset.data(bin30.dfm14.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm14_wells)
pdfcyc_dfm15_d2_7 <- subset.data(bin30.dfm15.190327.at, idate, itime, 0800, 2, 7, 'norm', 'running', pdfcyc_dfm15_wells)


pdfcyc_all_d2_7 <- combine_days(1, 
                                pdfcyc_dfm6_d2_7, 
                                pdfcyc_dfm7_d2_7, 
                                pdfcyc_dfm8_d2_7, 
                                pdfcyc_dfm9_d2_7, 
                                pdfcyc_dfm10_d2_7, 
                                pdfcyc_dfm11_d2_7, 
                                pdfcyc_dfm12_d2_7, 
                                pdfcyc_dfm13_d2_7, 
                                pdfcyc_dfm14_d2_7, 
                                pdfcyc_dfm15_d2_7)

pdfcyc_dfm6_d7_8 <- subset.data(bin30.dfm6.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm6_wells)
pdfcyc_dfm7_d7_8 <- subset.data(bin30.dfm7.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm7_wells)
pdfcyc_dfm8_d7_8 <- subset.data(bin30.dfm8.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm8_wells)
pdfcyc_dfm9_d7_8 <- subset.data(bin30.dfm9.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm9_wells)
pdfcyc_dfm10_d7_8 <- subset.data(bin30.dfm10.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm10_wells)
pdfcyc_dfm11_d7_8 <- subset.data(bin30.dfm11.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm11_wells)
pdfcyc_dfm12_d7_8 <- subset.data(bin30.dfm12.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm12_wells)
pdfcyc_dfm13_d7_8 <- subset.data(bin30.dfm13.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm13_wells)
pdfcyc_dfm14_d7_8 <- subset.data(bin30.dfm14.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm14_wells)
pdfcyc_dfm15_d7_8 <- subset.data(bin30.dfm15.190327.at, idate, itime, 0800, 7, 8, 'norm', 'running', pdfcyc_dfm15_wells)


pdfcyc_all_d7_8 <- combine_days(1, 
                                pdfcyc_dfm6_d7_8, 
                                pdfcyc_dfm7_d7_8, 
                                pdfcyc_dfm8_d7_8, 
                                pdfcyc_dfm9_d7_8, 
                                pdfcyc_dfm10_d7_8, 
                                pdfcyc_dfm11_d7_8, 
                                pdfcyc_dfm12_d7_8, 
                                pdfcyc_dfm13_d7_8, 
                                pdfcyc_dfm14_d7_8, 
                                pdfcyc_dfm15_d7_8)

# plot production
day_meanbehav_plot(pdfcyc_all_d2_7, 5, 1, 'pdfcyc_LD_d2_7', 'white')
day_meanbehav_plot(pdfcyc_all_d7_8, 5, 1, 'pdfcyc_DD_d7_8', 'grey80')

pdfiso_dfm6_aiPS <- AI_phase_score(bin30.dfm6.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm6_wells)
pdfiso_dfm7_aiPS <- AI_phase_score(bin30.dfm7.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm7_wells)
pdfiso_dfm8_aiPS <- AI_phase_score(bin30.dfm8.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm8_wells)
pdfiso_dfm9_aiPS <- AI_phase_score(bin30.dfm9.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm9_wells)
pdfiso_dfm10_aiPS <- AI_phase_score(bin30.dfm10.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm10_wells)
pdfiso_dfm11_aiPS <- AI_phase_score(bin30.dfm11.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm11_wells)
pdfiso_dfm12_aiPS <- AI_phase_score(bin30.dfm12.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm12_wells)
pdfiso_dfm13_aiPS <- AI_phase_score(bin30.dfm13.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm13_wells)
pdfiso_dfm14_aiPS <- AI_phase_score(bin30.dfm14.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm14_wells)
pdfiso_dfm15_aiPS <- AI_phase_score(bin30.dfm15.190327.at, 'pdfiso', idate, itime, etimeS, etimeE, 7, 8, pdfiso_dfm15_wells)

pdfiso_aiPS_all <- bind_rows(pdfiso_dfm6_aiPS, 
                             pdfiso_dfm7_aiPS, 
                             pdfiso_dfm8_aiPS, 
                             pdfiso_dfm9_aiPS,
                             pdfiso_dfm10_aiPS,
                             pdfiso_dfm11_aiPS,
                             pdfiso_dfm12_aiPS,
                             pdfiso_dfm13_aiPS,
                             pdfiso_dfm14_aiPS,
                             pdfiso_dfm15_aiPS)


isocyc_dfm6_PS <- phaseshift_genotype_time(bin30.dfm6.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm6_wells)
isocyc_dfm7_PS <- phaseshift_genotype_time(bin30.dfm7.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm7_wells)
isocyc_dfm8_PS <- phaseshift_genotype_time(bin30.dfm8.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm8_wells)
isocyc_dfm9_PS <- phaseshift_genotype_time(bin30.dfm9.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm9_wells)
isocyc_dfm10_PS <- phaseshift_genotype_time(bin30.dfm10.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm10_wells)
isocyc_dfm11_PS <- phaseshift_genotype_time(bin30.dfm11.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm11_wells)
isocyc_dfm12_PS <- phaseshift_genotype_time(bin30.dfm12.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm12_wells)
isocyc_dfm13_PS <- phaseshift_genotype_time(bin30.dfm13.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm13_wells)
isocyc_dfm14_PS <- phaseshift_genotype_time(bin30.dfm14.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm14_wells)
isocyc_dfm15_PS <- phaseshift_genotype_time(bin30.dfm15.190327.at, 'isocyc', idate, itime, etimeS, etimeE, pday, fday, 'norm', isocyc_dfm15_wells)

isocyc_PS_all <- bind_rows(isocyc_dfm6_PS, 
                          isocyc_dfm7_PS,
                          isocyc_dfm8_PS,
                          isocyc_dfm9_PS, 
                          isocyc_dfm10_PS,
                          isocyc_dfm11_PS,
                          isocyc_dfm12_PS,
                          isocyc_dfm13_PS,
                          isocyc_dfm14_PS,
                          isocyc_dfm15_PS
                          )

