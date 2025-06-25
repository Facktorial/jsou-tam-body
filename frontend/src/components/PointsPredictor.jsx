import { useState } from 'react';

const PointsPredictor = ({ serverData, BP, category, rankingType }) => {
  const [estimatedWinTime, setEstimatedWinTime] = useState(30);
  const [estimatedTime, setEstimatedTime] = useState(60);
  
  if (!serverData || !BP || !category || !rankingType) return null;

  const KS = serverData.eventInfo.eventKS;
  const KZ = serverData.eventInfo.eventKZ;

  const calculatePointsGoal = () => {
    if (KS === undefined || KS === null)  return 'N/A';
    if (KZ === undefined || KZ === null)  return 'N/A';
    
    const result = (2 - estimatedTime/estimatedWinTime) * BP * KZ * (1 - KS); 
    
    return Math.max(result.toFixed(0), 0);
  };

  const calculatePointsPerSecond = () => {
    if (KS === undefined || KS === null)  return 'N/A';
    if (KZ === undefined || KZ === null)  return 'N/A';
    
    const coef = BP * KZ * (1 - KS);
    const timeSecs = estimatedWinTime * 60;
    const result = -coef/timeSecs;
    
    return result.toFixed(2);
  };

  const handleTimeChange = (e) => {
    setEstimatedTime(parseFloat(e.target.value));
  };

  const handleTimeWinChange = (e) => {
    setEstimatedWinTime(parseFloat(e.target.value));
  };
  
  const pointsLoss = calculatePointsPerSecond();
  const pointsResult = calculatePointsGoal();

  return (
    <div className="bg-gray-900 p-4 rounded-lg border border-yellow-600 mt-4">
      <div className="text-yellow-400 font-mono mb-4">
        <h4 className="font-semibold text-lg mb-2">Points Predictor</h4>
        
        {/* Display server data info */}
        <div className="grid grid-cols-3 gap-4 mb-4 text-sm">
          <div>
            <span className="text-yellow-400">KS:</span>
            <span className="ml-2 text-yellow-200">{KS !== undefined && KS !== null ? KS.toFixed(2) : 'N/A'}</span>
          </div>
          <div>
            <span className="text-yellow-400">KZ:</span>
            <span className="ml-2 text-yellow-200">{KZ.toFixed(2) || 'N/A'}</span>
          </div>
          <div>
            <span className="text-yellow-400">Coef:</span>
            <span className="ml-2 text-yellow-200">{BP || 'N/A'}</span>
          </div>
        </div>
        
        {/* Time input slider */}
        <div className="mb-4">
          <label className="block text-blue-300 font-mono mb-2">
            Estimated Winner Time (minutes):
          </label>
          <div className="flex items-center gap-4">
            <input
              type="range"
              min="10"
              max="180"
              step="0.5"
              value={estimatedWinTime}
              onChange={handleTimeWinChange}
              className="flex-1 h-2 bg-gray-700 rounded-lg appearance-none cursor-pointer slider"
              style={{
                background: `linear-gradient(to right, #3b82f6 0%, #3b82f6 ${((estimatedWinTime - 10) / 170) * 100}%, #374151 ${((estimatedWinTime - 10) / 170) * 100}%, #374151 100%)`,
              }}
            />
            <input
              type="number"
              min="10"
              max="180"
              step="0.2"
              value={estimatedWinTime}
              onChange={handleTimeWinChange}
              className="bg-gray-800 px-3 py-1 rounded border border-blue-500 min-w-[80px] text-center text-blue-200 font-mono font-semibold focus:outline-none focus:ring-2 focus:ring-blue-400"
            />
          </div>
        </div>
        
        {/* Time input slider */}
        <div className="mb-4">
          <label className="block text-blue-300 font-mono mb-2">
            Estimated My Time (minutes):
          </label>
          <div className="flex items-center gap-4">
            <input
              type="range"
              min="10"
              max="180"
              step="0.5"
              value={estimatedTime}
              onChange={handleTimeChange}
              className="flex-1 h-2 bg-gray-700 rounded-lg appearance-none cursor-pointer slider"
              style={{
                background: `linear-gradient(to right, #3b82f6 0%, #3b82f6 ${((estimatedTime - 10) / 170) * 100}%, #374151 ${((estimatedTime - 10) / 170) * 100}%, #374151 100%)`
              }}
            />
            <input
              type="number"
              min="10"
              max="180"
              step="0.2"
              value={estimatedTime}
              onChange={handleTimeChange}
              className="no-arrows bg-gray-800 px-3 py-1 rounded border border-blue-500 min-w-[80px] text-center text-blue-200 font-mono font-semibold focus:outline-none focus:ring-2 focus:ring-blue-400"
            />
          </div>
        </div>
        
        {/* Result display */}
        <div className="bg-gray-800 p-3 rounded border border-blue-400">
          { KS !== 0.00 ?
            (<div className="text-blue-300 font-mono mb-1 text-xs">
              Formula: (2 - {estimatedTime}/{estimatedWinTime}) × {BP || 'coef'} × (1 - {KS.toFixed(2)}) × {KZ.toFixed(2)} 
              <div className="text-xs text-blue-600 mt-2">
                <details>
                  <summary className="cursor-pointer">This is just aproximation</summary>
                  <ul className="mt-1 ml-4">
                      <li className="text-blue-500">• Actual rules working with:
                      <span font-mono>
                          (1 - {KS.toFixed(2)} × (U - 1)/(N -1))
                      </span></li>
                      <li className="text-blue-500">• Ratio in this element has range between 0 and 1</li>
                      <li className="text-blue-500">• In my math I am assuming worst case scenario</li>
                  </ul>
                </details>
              </div>
            </div>
            )
            :
            (<div className="text-blue-300 font-mono mb-1 text-xs">
              Formula: (2 - {estimatedTime}/{estimatedWinTime}) × {BP || 'coef'} × {KZ.toFixed(2)} 
            </div>)
          }
          <div className="text-blue-200 font-mono">
            <span className="text-blue-300">Regrets:</span>
            <span className="ml-2 text-blue-100 font-bold text-lg">
              {pointsLoss}
            </span>
            <span className="ml-1 text-blue-400 text-sm"> points/sec</span>
          </div>
          <div className="text-blue-200 font-mono">
            <span className="text-blue-300">Estimated result:</span>
            <span className="ml-2 text-blue-100 font-bold text-lg">
              {pointsResult}
            </span>
          </div>
        </div>
      </div>
      
      <style jsx>{`
        .slider::-webkit-slider-thumb {
          appearance: none;
          height: 20px;
          width: 20px;
          border-radius: 50%;
          background: #3b82f6;
          cursor: pointer;
          border: 2px solid #1e40af;
        }
        
        .slider::-moz-range-thumb {
          height: 20px;
          width: 20px;
          border-radius: 50%;
          background: #3b82f6;
          cursor: pointer;
          border: 2px solid #1e40af;
        }
        
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
          -webkit-appearance: none;
          margin: 0;
        }
        
        input[type=number] {
          -moz-appearance: textfield;
        }
      `}</style>
    </div>
  );
};

export default PointsPredictor;
