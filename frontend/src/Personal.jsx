import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import LegendForm from "./components/Legend";
import EventList from "./components/EventList";
import RunnerRankingsHistogram from "./components/RunnerRankingsHistogram";


const Personal = () => {
  const navigate = useNavigate();
  const [serverData, setServerData] = useState(null);
  const [racesData, setRacesData] = useState(null);
  const [loading, setLoading] = useState({
    runnerLoaded: false,
    racesLoaded: false
  });
  const [selectedRanking, setSelectedRanking] = useState(0);
  const [availableOptions, setAvailableOptions] = useState(null);
  const [requestStatusEvents, setRequestStatusEvents] = useState("idle");
  // const [serverUrl, setServerUrl] = useState('https://empowering-connection-dev.up.railway.app');
  // const [serverUrl, setServerUrl] = useState('http://localhost:8000');
  const [serverUrl, setServerUrl] = useState(process.env.REACT_APP_API_URL);

  const handleDataReceived = (data) => {
    setServerData(data);
    setLoading(prev => ({ ...prev, runnerLoaded: false }));
    console.log('Received runner data:', data);
  };

  const handleRacesReceived = (data) => {
    setRacesData(data);
    setLoading(prev => ({ ...prev, racesLoaded: false }));
    console.log('Received races data:', data);
    setRequestStatusEvents('success')
  };

  const gender = serverData?["standings"]["runnerName"] !== "unknown - FIXME "
    ? "M" : "F" : "";

  const rankingsData = serverData
    ? Object.values(serverData["rankingtypes"]["Data"])
    : {};

  const events = racesData
    ? racesData["events"]
    : {};

  return (
    <div 
      className="min-h-screen p-8"
      style={{
        background: 'linear-gradient(135deg, #f5f1e8 0%, #ede4d3 50%, #e8ddc7 100%)',
        backgroundImage: `
          radial-gradient(circle at 25% 25%, rgba(255,255,255,0.2) 0%, transparent 50%),
          radial-gradient(circle at 75% 75%, rgba(0,0,0,0.05) 0%, transparent 50%),
          linear-gradient(45deg, rgba(255,255,255,0.1) 25%, transparent 25%),
          linear-gradient(-45deg, rgba(255,255,255,0.1) 25%, transparent 25%)
        `,
        backgroundSize: '200px 200px, 200px 200px, 40px 40px, 40px 40px'
      }}
    >
      <div className="max-w-md mx-auto bg-black rounded-lg shadow-2xl border border-yellow-500 backdrop-blur-sm">
        <div className="p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-2xl font-bold text-yellow-400">Mám nějaké body?</h2>
            <button
              className="text-xs px-2 py-1 bg-gray-800 text-yellow-200 border border-yellow-600 rounded font-mono hover:bg-yellow-600 hover:text-black"
              onClick={() => navigate("/") }
            >
              Jsou tam body?
            </button>
          </div>

          <LegendForm 
            serverUrl={serverUrl}
            onDataReceived={handleDataReceived}
            onRacesReceived={handleRacesReceived}
            serverData={serverData}
            racesData={racesData}
            loading={loading}
            setLoading={setLoading}
            requestStatusEvents={requestStatusEvents}
            setRequestStatusEvents={setRequestStatusEvents}
            setAvailableOptions={setAvailableOptions}
          />
      
          {serverData && (
            <RunnerRankingsHistogram
              runnerData={serverData["standings"][gender]}
              rankingsData={rankingsData}
              rankingType={selectedRanking}
              setRankingType={setSelectedRanking}
              requestStatusEvents={requestStatusEvents}
              setRequestStatusEvents={setRequestStatusEvents}
            />
          )}

          {(serverData && !racesData) && (
            <div class="mb-4 mt-4">
            <div className="text-yellow-400 text-center py-4">
              Loading runner data...
            </div></div>
          )}
      
          {racesData && (
            <select
              value={selectedRanking}
              onChange={(e) => setSelectedRanking(e.target.value)}
              disabled={requestStatusEvents !== 'success'}
              className={`w-full p-2 mt-2 mb-2 rounded font-mono focus:outline-none focus:ring-2 ${
                requestStatusEvents !== 'success' 
                  ? 'bg-gray-700 border border-gray-500 text-gray-400 cursor-not-allowed' 
                  : 'bg-gray-900 border border-yellow-600 text-yellow-200 focus:ring-yellow-500 hover:border-yellow-500'
              }`}
            >
              <option value="" className={requestStatusEvents !== 'success' ? 'bg-gray-700 text-gray-400' : 'bg-gray-900 text-yellow-700'}>
                {requestStatusEvents !== 'success' ? 'Check Race ID and H/D first' : 'Select Ranking Type'}
              </option>
              {availableOptions?.rankingTypes?.map(rank => (
                <option key={rank.value} value={rank.value} className="bg-gray-900 text-yellow-200">
                  {rank.label}
                </option>
              ))}
            </select>
          )}

          {racesData && (
            <div className="mt-6">
                <h4 className="text-yellow-400 font-mono">
                  Results history:
                </h4>
                <div className="text-yellow-400 font-mono">
                  {availableOptions?.rankingTypes?.find(rank =>
                    rank.value == selectedRanking)?.label
                  }
                </div>
  
              <EventList
                events={racesData["events"]}
                rankings={availableOptions?.rankingTypes}
                selectedRanking={selectedRanking}
              />

              <div className="mt-6 p-4 bg-gray-900 rounded border border-yellow-600">
                <h4 className="text-yellow-400 font-bold mb-2">Legend:</h4>
                <div className="flex flex-wrap gap-3 text-sm">
                  <div className="flex items-center space-x-2">
                    <div className="w-3 h-4 bg-red-500 rounded-sm"></div>
                    <span className="text-yellow-200">Expire this month</span>
                  </div>
                  <div className="flex items-center space-x-2">
                    <div className="w-3 h-4 bg-yellow-500 rounded-sm"></div>
                    <span className="text-yellow-200">About to expire</span>
                  </div>
                  <div className="flex items-center space-x-2">
                    <div className="w-3 h-4 bg-green-500 rounded-sm"></div>
                    <span className="text-yellow-200">Will be counted for a while</span>
                  </div>
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>);
}

export default Personal;
      
     //     {serverData && (
     //       <div className="mt-6 p-4 bg-gray-900 rounded border border-yellow-600">
     //         <h3 className="text-yellow-400 font-bold mb-2">Runner Data:</h3>
     //         <pre className="text-yellow-200 text-sm overflow-auto">
     //           {JSON.stringify(serverData, null, 2)}
     //         </pre>
     //       </div>
     //     )}
