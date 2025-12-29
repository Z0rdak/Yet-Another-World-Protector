package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.platform.services.event.FlagEventDispatcher;
import net.neoforged.neoforge.common.NeoForge;

public final class NeoForgeFlagEventDispatcher implements FlagEventDispatcher {

    @Override
    public <T extends FlagEvent> boolean post(T event) {
        if (event instanceof FlagEvent.Add add) {
            NeoForgeFlagEvent.Add forgeEvent = new NeoForgeFlagEvent.Add(add);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || add.isCanceled();
        }
        else if (event instanceof FlagEvent.Remove remove) {
            NeoForgeFlagEvent.Remove forgeEvent = new NeoForgeFlagEvent.Remove(remove);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || remove.isCanceled();
        }
        else if (event instanceof FlagEvent.UpdateFlagMessage update) {
            NeoForgeFlagEvent.UpdateFlagMessage forgeEvent = new NeoForgeFlagEvent.UpdateFlagMessage(update);
            NeoForge.EVENT_BUS.post(forgeEvent);
            // non-cancelable, no need to check isCanceled
            return false;
        }
        return false;
    }

    @Override
    public boolean post(FlagCheckRequest event) {
        var forgeFlagCheck = new NeoForgeFlagCheckRequest(event);
        NeoForge.EVENT_BUS.post(forgeFlagCheck);
        return forgeFlagCheck.isCanceled() || event.isCanceled();
    }

    @Override
    public FlagCheckResult post(FlagCheckResult result) {
        var forgeFlagResult = NeoForgeFlagCheckResult.asEvent(result);
        NeoForge.EVENT_BUS.post(forgeFlagResult);
        return NeoForgeFlagCheckResult.asNonEvent(forgeFlagResult);
    }
}
