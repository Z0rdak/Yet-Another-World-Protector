package de.z0rdak.yawp.platform.services.event;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.api.events.flag.FlagEvent;

public interface FlagEventDispatcher {

    <T extends FlagEvent> void post(T event);

    boolean post(FlagCheckRequest event);

    FlagCheckResult post(FlagCheckResult result);
}