package de.z0rdak.yawp.platform.services;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.RegionEvent;

public interface IEventHelper {
    
    boolean post(FlagCheckEvent event);
    FlagCheckResult post(FlagCheckResult event);
    boolean post(RegionEvent event);

    void post(FlagEvent event);
}
