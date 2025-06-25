import React from 'react';

// Details List Component
const DetailsList = ({ showDetails, detailsList, getRunners }) => {
  if (!showDetails) return null;

  return (
    <div className="mt-4 border-t border-yellow-600 pt-3">
        <div className="space-y-1">
          {getRunners().map((runner, index) => (
            <div key={index} className="flex justify-between items-center text-xs">
              <span className="text-yellow-300">
                {index + 1}. {runner.racerName}
              </span>
              <div className="flex gap-2">
                <span className="text-yellow-400 font-mono">
                  {runner.racerCoef}
                </span>
                <span className="text-yellow-500">
                  (#{runner.racerRanking})
                </span>
              </div>
            </div>
          ))}
      </div>
    </div>
  );
};

export default DetailsList;
