clear
[CsScenarioNames, SCrossingRoadUserConstants] = GetScenarioConstants;
sScenarioName = 'Pedestrian crossing UK';
idxScenario = find(strcmp(CsScenarioNames, sScenarioName)); idxScenario

% set approaching road user constants
SApproachingRoadUserConstants = GetBaseApproachingRoadUserConstants;
SApproachingRoadUserConstants.initialSpeed = 50 / 3.6; % m/s
SApproachingRoadUserConstants.initialTTA = 4.58; % s
SApproachingRoadUserConstants.decelerationOnsetTTA = 4.58; % s
SApproachingRoadUserConstants.stopAtDistanceFromConflict = 4;
SApproachingRoadUserConstants.eHMIOnsetTTA = NaN; % no eHMI
SApproachingRoadUserConstants.desiredPostEncroachmentTime = 2; % s
SApproachingRoadUserConstants.maxAcceleration = 2; % m/s^2

%% get a scenario description string to use in the plots
sScenarioDescription = sprintf('%s\nv_{init} = %.1f km/h; TTA_{init} = %.1f s; D_{stop} = %.1f m', ...
    sScenarioName, SApproachingRoadUserConstants.initialSpeed * 3.6, ...
    SApproachingRoadUserConstants.initialTTA, ...
    SApproachingRoadUserConstants.stopAtDistanceFromConflict);

%% simulation constants
SSimulationConstants.timeStep = 1/30; % second
SSimulationConstants.endTime = 30; % second
SSimulationConstants.nSimulations = 50; % s (step? 50 counts?)

VTimeStamp = 0:SSimulationConstants.timeStep:SSimulationConstants.endTime;
SSimulationResults.VTimeStamp = VTimeStamp;

% get the non-adapted approach behaviour of the approaching road user, and
% store it also in the output structure
SNonAdaptedApproachingRoadUserBehaviour = ...
  SApproachingRoadUserConstants.fGetApproachBehaviour(...
  VTimeStamp, SApproachingRoadUserConstants);

SSimulationResults.SApproachingRoadUserWithoutAdaptation = ...
SNonAdaptedApproachingRoadUserBehaviour;

SSimulationResults.VCrossingOnsetTimePDF = ...
   SCrossingRoadUserConstants.fGetCrossingOnsetTimePDF(...
   VTimeStamp, SCrossingRoadUserConstants(idxScenario), SApproachingRoadUserConstants, ...
   SNonAdaptedApproachingRoadUserBehaviour);

%% function VCrossingOnsetTimePDF = GetModelCrossingOnsetTimePDF(VTimeStamp, ...
%%   SCrossingRoadUserConstants, SApproachingRoadUserConstants, ...
%%   SApproachingRoadUserBehaviour)
% use the threshold distribution model (TDM) to predict a crossing onset
% time distribution

 VLongitudinalDistance = ...
     GetLongDistanceFromCrosserInitialPosToApproacherFront(...
     SCrossingRoadUserConstants(idxScenario), SApproachingRoadUserConstants, ...
     SApproachingRoadUserBehaviour);
%%  save "-ascii" "VLongitudinalDistance.txt" VLongitudinalDistance

%% VXdot_tmp =  SApproachingRoadUserBehaviour.VXdot;
%% VbYieldingeHMI_tmp = SApproachingRoadUserBehaviour.VbYieldingeHMI;
%% SModelSpecificConstants =  SCrossingRoadUserConstants.SModelSpecificConstants;
%% save "-ascii" "VTimeStamp.txt" VTimeStamp
%% save "-ascii" "VLongitudinalDistance.txt" VLongitudinalDistance
%% save "-ascii" "SApproachingRoadUserBehaviour_VXdot.txt" VXdot_tmp
%% save "-ascii" "SApproachingRoadUserBehaviour_VbYieldingeHMI.txt" VbYieldingeHMI_tmp
%% save "-mat7-binary" "SModelSpecificConstants.bin" SModelSpecificConstants


VCrossingOnsetTimePDF = threshold_distr(VTimeStamp, ...
  VLongitudinalDistance, -SApproachingRoadUserBehaviour.VXdot, ...
  SApproachingRoadUserBehaviour.VbYieldingeHMI, ...
  SCrossingRoadUserConstants.SModelSpecificConstants);

save "-ascii" "VCrossingOnsetTimePDF.txt" VCrossingOnsetTimePDF

VCrossingOnsetCDF = cumtrapz(VTimeStamp, VCrossingOnsetTimePDF);
save "-ascii" "VCrossingOnsetCDF.txt" VCrossingOnsetCDF

VCumulativeCrossingOnsetProbabilities = linspace(0, 1, SSimulationConstants.nSimulations+2);
save "-ascii" "VCumulativeCrossingOnsetProbabilities0.txt" VCumulativeCrossingOnsetProbabilities

VCumulativeCrossingOnsetProbabilities = VCumulativeCrossingOnsetProbabilities(2:end-1);
save "-ascii" "VCumulativeCrossingOnsetProbabilities1.txt" VCumulativeCrossingOnsetProbabilities
