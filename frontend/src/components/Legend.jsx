import React, { useState } from "react";
import StatusMessages from "./StatusMessage"

const LegendForm = ({
  serverUrl,
  onDataReceived,
  onRacesReceived,
  serverData,
  racesData,
  loading,
  setLoading,
  requestStatusEvents,
  setRequestStatusEvents,
  setAvailableOptions,
  runnerId,
  setRunnerId
}) => {
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);

  const extractRankingTypesFromResponse = (data) => {
    if (!data?.rankingtypes?.Data) return [];
    
    const rankingTypes = Object.values(data.rankingtypes.Data).map(rankType => ({
      value: rankType.ID - 1,
      label: rankType.NameEN
    }));
    
    return rankingTypes;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    setLoading({
      runner: true,
      races: true
    });

    onDataReceived(null);
    onRacesReceived(null);

    if (!runnerId.trim()) {
      setError('Please enter a runner RegNo');
      return;
    }

    const runnerRegno = runnerId.toUpperCase();

    setIsLoading(true);
    setError(null);

    const requestUrl = `${serverUrl}/api/backgroundcheck/standings/${runnerRegno}`;
    const requestUrlEvents = `${serverUrl}/api/backgroundcheck/${runnerRegno}`;
    console.log('Sending runner request to:', requestUrl);
    console.log('Sending runner request to:', requestUrlEvents);

    try {
      const runnerResponse = fetch(`${requestUrl}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });
      const racesResponse = fetch(`${requestUrlEvents}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      runnerResponse
        .then(response => {
          if (!response.ok) throw new Error('Runner data request failed');
          return response.json();
        })
        .then(data => {
          onDataReceived(data);
          if (data) {
            const rankingTypes = extractRankingTypesFromResponse(data);
            //console.log('rankingTypes:');
            //console.table(rankingTypes);
            const updatedOptions = {
              success: true,
              rankingTypes: rankingTypes
            };
            setAvailableOptions(updatedOptions);
          }
        })
        .catch(error => {
          console.error('Error fetching runner data:', error);
          setLoading(prev => ({ ...prev, runner: false }));
          onRacesReceived({ error : error, success : false });
        });

      // Handle second request completion
      racesResponse
        .then(response => {
          if (!response.ok) throw new Error('Races data request failed');
          return response.json();
        })
        .then(data => {
          onRacesReceived(data);
        })
        .catch(error => {
          console.error('Error fetching races data:', error);
          setLoading(prev => ({ ...prev, races: false }));
          onRacesReceived({ error : error, success : false });
        });

    } catch (error) {
      console.error('Error fetching data:', error);
      setError(`Failed to connect to server: ${error.message}`);
      setLoading({
        runner: false,
        races: false
      });
      setRequestStatusEvents('error');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="mb-4 space-y-3">
      <form onSubmit={handleSubmit}>
        <div className="flex gap-2">
          <input
            type="text"
            placeholder="Runner RegNo (try MOV9500)"
            value={runnerId}
            onChange={(e) => setRunnerId(e.target.value)}
            className="flex-1 min-w-0 sm:flex-1 md:w-48 lg:w-64 p-2 bg-gray-900 border border-yellow-600 rounded text-yellow-200 placeholder-yellow-700 focus:outline-none focus:ring-2 focus:ring-yellow-500 font-mono"
          />
          <button
            type="submit"
            disabled={!runnerId.trim() || (isLoading.runner || loading.races)}
            className="px-4 py-2 bg-yellow-600 text-black rounded hover:bg-yellow-500 disabled:bg-gray-700 disabled:text-gray-400 font-mono font-bold"
          >
            {(isLoading.runner && isLoading.races) ? 'Loading...' : 'Explore'}
          </button>
        </div>
      </form>

      {(error || serverData?.success == false) && (
        <StatusMessages
          requestStatus={serverData.error}
          id={runnerId}
          response={serverData.message}
          availableOptions={null}
          serverData={serverData}
        />
      )}

    </div>
  );
};

export default LegendForm;
          //value={id}
          //onChange={(e) => setId(e.target.value)}

          //onClick={() => handleInitialRequest(id, hOrDCategory, forcingAge)}
          //disabled={!id.trim() || !hOrDCategory || isInitialLoading}
          //{isInitialLoading ? checkingText : checkingButtonText}
