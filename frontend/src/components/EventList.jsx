import React, { useState, useMemo } from 'react';

const FilteredEventsCount = 4;

const EventList = ({ events, rankings, selectedRanking }) => {
  const [filter, setFilter] = useState('all');

  const eventLink = "https://oris.orientacnisporty.cz/Zavod?id="

  const parseRule = (label) => {
    // Example: "Sprint ranking - 24 months, 5 events"
    // Example: "Lesni ranking - 12 months, 3 events"
    if (label === "Standard ranking") {
      return {
        raceType: "All",
        durationInMonths: 12,
        eventCount: 8
      };
    }
    
    const regex = /^(Sprint|Forrest)\s+ranking\s*-\s*(\d+)\s+months?,\s*(\d+)\s+events?$/i;
    const match = label.match(regex);
    
    if (!match) {
      throw new Error(`Invalid category format: ${label}`);
    }

    return {
      raceType: match[1],
      durationInMonths: parseInt(match[2], 10),
      eventCount: parseInt(match[3], 10)
    };
  };

  const rules = rankings.map((item) => parseRule(item.label));

  const getMonthsDifference = (startDate, endDate) => {
    const years = endDate.getFullYear() - startDate.getFullYear();
    const months = endDate.getMonth() - startDate.getMonth();
    
    let totalMonths = years * 12 + months;
    
    return totalMonths;
  };

  const isInCurrentMonth = (date) => {
    const now = new Date();
    return date.getFullYear() === now.getFullYear() && 
           date.getMonth() === now.getMonth();
  };

  const checkValidDuration = (dateStr, rule) => {
    try {
      const targetDate = new Date(dateStr);
      const currentDate = new Date();
      
      if (isNaN(targetDate.getTime())) {
        return 'invalid';
      }
      
      const expirationDate = new Date(targetDate);
      expirationDate.setMonth(expirationDate.getMonth() + rule.durationInMonths);
      
      const monthsUntilExpiration = getMonthsDifference(currentDate, expirationDate);
      
      if (monthsUntilExpiration == 0) { return 'expired'; }
      else if (monthsUntilExpiration < 0) { return 'invalid'; }
      else if (monthsUntilExpiration < 3) { return 'warning'; }
      else { return 'valid'; }

    } catch (error) {
          console.error("Caught in outer checkValidDuration:", error);
      return 'invalid';
    }
  };

  const getValidDurationColor = (date, rule) => {
    const status = checkValidDuration(date, rule);

    switch (status) {
      case 'expired':
        return 'bg-red-500';
      case 'warning':
        return 'bg-yellow-500';
      case 'valid':
        return 'bg-green-500';
      default:
        return 'bg-gray-500';
    }
  };

  const formatDate = (dateStr) => {
    const date = new Date(dateStr);
    return date.toLocaleDateString('cs-CZ', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric'
    });
  };

  const getEventStyle = (index, disciplineID) => {
    if (index < rules[selectedRanking].eventCount) {
      if (disciplineID === 1 || disciplineID === 2) {
        return 'bg-white bg-opacity-90 border-yellow-600 text-gray-900 relative overflow-hidden';
      }
      return 'bg-white bg-opacity-90 border-yellow-600 text-gray-900 relative overflow-hidden';
    } else {
      return 'bg-gray-600 bg-opacity-80 border-gray-500 text-gray-300';
    }
  };

  const getStripeOverlay = (index, disciplineID) => {
    if (index < 8 && (disciplineID === 1 || disciplineID === 2)) {
      return (
        <div 
          className="absolute inset-0 pointer-events-none rounded-lg"
          style={{
            background: 'repeating-linear-gradient(-30deg, transparent, transparent 18px, rgba(34, 197, 94, 0.1) 2px, rgba(34, 197, 94, 0.1) 20px)',
            zIndex: 1
          }}
        />
      );
    }
    else if (index < 8 && (disciplineID === 3)) {
       return (
         <div 
           className="absolute inset-0 pointer-events-none rounded-lg"
           style={{
             background: 'repeating-linear-gradient(30deg, transparent, transparent 18px, rgba(255, 153, 19, 0.1) 2px, rgba(255, 153, 19, 0.1) 20px)',
             zIndex: 1
           }}
         />
       );
    }
    return null;
  };

  const isDisciplineMatch = (evDisciplineId, currentRule) => {
    if (currentRule === "All") {
      return true;
    }
  
    if (currentRule === "Sprint") {
      return evDisciplineId === 3;
    }
  
    if (currentRule === "Forrest") {
      return evDisciplineId === 1 || evDisciplineId === 2;
    }
  
    return false; // fallback if rule is unknown
  };

  const filteredEvents = useMemo(() => {
    if (!events || events.length === 0) return [];
    
    const valids = events.filter(ev => {
      let count = 0;
      const rule = rules[selectedRanking];

      const status = checkValidDuration(ev.date, rule);
  
      if (!isDisciplineMatch(ev.disciplineID, rule.raceType)) return false;
      if (status === "invalid") { return false; }
      if (filter === 'all') return true;
      return status === filter;
    });
    console.table(valids);
    return valids.slice(0, rules[selectedRanking].eventCount + FilteredEventsCount);
  }, [events, rules[0], filter]);
  
  if (!filteredEvents || filteredEvents.length === 0) {
    return null;
  }

          //{getStripeOverlay(index, event.disciplineID)}
                //{/* Icon placeholder */}
                //<div className={`w-8 h-8 rounded-full flex items-center justify-center ${
                //  index < 8 ? 'bg-gray-200' : 'bg-gray-700'
                //}`}>
                //  <div className="w-4 h-4 bg-gray-400 rounded-sm opacity-50"></div>
                //</div>
  return (
    <div className="mt-6">
      <div className="space-y-2">
        {filteredEvents.map((event, index) => (
          <a
            key={event.eventId}
            href={`${eventLink}${event.eventId}`}
            className={`block p-3 rounded-lg border-2 ${getEventStyle(index, event.disciplineID)} transition-all duration-200 hover:shadow-lg hover:scale-[1.02] cursor-pointer`}
          >
            <div className="flex items-center justify-between" style={{ position: 'relative', zIndex: 2 }}>
              <div className="flex items-center space-x-3">
                
                {/* Discipline color flag */}
                <div
                  className={`w-3 h-6 rounded-sm ${getValidDurationColor(event.date, rules[selectedRanking])}`}
                  title={event.discipline}
                />
                
                <div className="flex-1">
                  <div className="flex items-center space-x-2">
                    <span className="font-semibold text-sm">
                      {event.name}
                    </span>
                    <span className={`text-xs px-2 py-1 rounded`}>
                      {event.discipline}
                    </span>
                  </div>
                  
                  <div className="flex items-center space-x-4 mt-1">
                    <span className="text-sm">
                      {formatDate(event.date)}
                    </span>
                    <span className={`text-sm font-mono ${
                      index < 8 ? 'text-gray-600' : 'text-gray-400'
                    }`}>
                      {event.points} bodů
                    </span>
                  </div>
                </div>
              </div>
              
              {/* Event ranking indicator */}
              <div className="text-right">
                <div className={`text-xs ${
                  index < 8 ? 'text-gray-500' : 'text-gray-400'
                }`}>
                  #{index + 1}
                </div>
              </div>
            </div>
          </a>
        ))}
      </div>
    </div>
  );
};

export default EventList;
