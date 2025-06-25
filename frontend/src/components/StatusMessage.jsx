const StatusMessages = ({ requestStatus, id, mockInitialResponse, availableOptions, serverData, getRunners }) => {
  if (requestStatus === 'error') {
    return (
      <div className="p-3 bg-red-900 border border-red-600 rounded">
        <p className="text-red-400 font-mono">
          {mockInitialResponse[id]?.error || 'Failed to connect to server'}
        </p>
      </div>
    );
  }

  if (requestStatus === 'success' && availableOptions) {
    if (!serverData) return null;
    const name = serverData.eventInfo.eventName;
    const disc = serverData.eventInfo.eventDiscipline;
    const plac = serverData.eventInfo.eventPlace;

    return (
      <div className="p-3 bg-green-900 border border-green-600 rounded">
        <p className="text-green-400 font-mono mb-3">{name}</p>
        <p className="text-green-400 font-mono mb-3">{disc}</p>
        <p className="text-green-400 font-mono mb-3">{plac}</p>
      </div>
    );
  }

  return null;
};

export default StatusMessages;
//<p className="text-green-400 font-mono mb-2">✓ Race ID and category verified - Options available</p>
