package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.platform.services.event.FlagEventDispatcher;
import net.minecraftforge.common.MinecraftForge;

public final class ForgeFlagEventDispatcher implements FlagEventDispatcher {

    @Override
    public <T extends FlagEvent> boolean post(T event) {
        if (event instanceof FlagEvent.Add add) {
            ForgeFlagEvent.Add forgeEvent = new ForgeFlagEvent.Add(add);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || add.isCanceled();
        }
        else if (event instanceof FlagEvent.Remove remove) {
            ForgeFlagEvent.Remove forgeEvent = new ForgeFlagEvent.Remove(remove);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || remove.isCanceled();
        }
        else if (event instanceof FlagEvent.UpdateFlagMessage update) {
            ForgeFlagEvent.UpdateFlagMessage forgeEvent = new ForgeFlagEvent.UpdateFlagMessage(update);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            // non-cancelable, no need to check isCanceled
            return false;
        }
        return false;
    }

    @Override
    public boolean post(FlagCheckRequest event) {
        var forgeFlagCheck = new ForgeFlagCheckRequest(event);
        MinecraftForge.EVENT_BUS.post(forgeFlagCheck);
        return forgeFlagCheck.isCanceled() || event.isCanceled();
    }

    @Override
    public FlagCheckResult post(FlagCheckResult result) {
        var forgeFlagResult = ForgeFlagCheckResult.asEvent(result);
        MinecraftForge.EVENT_BUS.post(forgeFlagResult);
        return ForgeFlagCheckResult.asNonEvent(forgeFlagResult);
    }
}
