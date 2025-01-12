package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.NeoForgeFlagEvent;
import de.z0rdak.yawp.api.events.region.*;
import de.z0rdak.yawp.platform.services.IEventHelper;
import net.neoforged.neoforge.common.NeoForge;

public class NeoForgeEventHelper implements IEventHelper {

    @Override
    public boolean post(FlagCheckEvent event) {
        NeoForgeFlagCheckEvent forgeEvent = new NeoForgeFlagCheckEvent(event);
        return NeoForge.EVENT_BUS.post(forgeEvent).isCanceled();
    }

    @Override
    public FlagCheckResult post(FlagCheckResult event) {
        NeoForgeFlagCheckResult forgeEvent = NeoForgeFlagCheckResult.asEvent(event);
        NeoForge.EVENT_BUS.post(forgeEvent);
        return NeoForgeFlagCheckResult.asNonEvent(forgeEvent);
    }

    @Override
    public boolean post(RegionEvent event) {
        if (event instanceof RegionEvent.Create) {
            NeoForgeRegionEvent.Create create = new NeoForgeRegionEvent.Create((RegionEvent.Create) event);
            return NeoForge.EVENT_BUS.post(create).isCanceled();
        }
        if (event instanceof RegionEvent.Rename) {
            NeoForgeRegionEvent.Rename rename = new NeoForgeRegionEvent.Rename((RegionEvent.Rename) event);
            return NeoForge.EVENT_BUS.post(rename).isCanceled();
        }
        if (event instanceof RegionEvent.Remove) {
            NeoForgeRegionEvent.Remove remove = new NeoForgeRegionEvent.Remove((RegionEvent.Remove) event);
            return NeoForge.EVENT_BUS.post(remove).isCanceled();
        }
        if (event instanceof RegionEvent.UpdateArea) {
            NeoForgeRegionEvent.UpdateArea updateArea = new NeoForgeRegionEvent.UpdateArea((RegionEvent.UpdateArea) event);
            return NeoForge.EVENT_BUS.post(updateArea).isCanceled();
        }
        return false;
    }

    @Override
    public RegionEvent.UpdateArea post(RegionEvent.UpdateArea event) {
        NeoForgeRegionEvent.UpdateArea forgeEvent = NeoForgeRegionEvent.UpdateArea.asEvent(event);
        NeoForge.EVENT_BUS.post(forgeEvent);
        return NeoForgeRegionEvent.UpdateArea.asNonEvent(forgeEvent);
    }


    @Override
    public void post(FlagEvent event) {
        if (event instanceof FlagEvent.AddFlagEvent) {
            NeoForgeFlagEvent.AddFlagEvent addFlagEvent = new NeoForgeFlagEvent.AddFlagEvent((FlagEvent.AddFlagEvent) event);
            NeoForge.EVENT_BUS.post(addFlagEvent);
        }
        if (event instanceof FlagEvent.RemoveFlagEvent) {
            NeoForgeFlagEvent.RemoveFlagEvent removeFlagEvent = new NeoForgeFlagEvent.RemoveFlagEvent((FlagEvent.RemoveFlagEvent) event);
            NeoForge.EVENT_BUS.post(removeFlagEvent);
        }
        if (event instanceof FlagEvent.UpdateFlagMessageEvent) {
            NeoForgeFlagEvent.UpdateFlagMessageEvent updateFlagMessageEvent = new NeoForgeFlagEvent.UpdateFlagMessageEvent((FlagEvent.UpdateFlagMessageEvent) event);
            NeoForge.EVENT_BUS.post(updateFlagMessageEvent);
        }
    }

    @Override
    public FlagEvent.UpdateFlagMessageEvent post(FlagEvent.UpdateFlagMessageEvent event) {
        NeoForgeFlagEvent.UpdateFlagMessageEvent forgeEvent = NeoForgeFlagEvent.UpdateFlagMessageEvent.asEvent(event);
        NeoForge.EVENT_BUS.post(forgeEvent);
        return NeoForgeFlagEvent.UpdateFlagMessageEvent.asNonEvent(forgeEvent);
    }
}
