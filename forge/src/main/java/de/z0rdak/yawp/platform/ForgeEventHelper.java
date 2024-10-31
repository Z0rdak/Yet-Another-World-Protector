package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.ForgeFlagEvent;
import de.z0rdak.yawp.api.events.region.*;
import de.z0rdak.yawp.platform.services.IEventHelper;
import net.minecraftforge.common.MinecraftForge;

public class ForgeEventHelper implements IEventHelper {

    @Override
    public boolean post(FlagCheckEvent event) {
        ForgeFlagCheckEvent forgeEvent = new ForgeFlagCheckEvent(event);
        return MinecraftForge.EVENT_BUS.post(forgeEvent);
    }

    @Override
    public FlagCheckResult post(FlagCheckResult event) {
        ForgeFlagCheckResult forgeEvent = ForgeFlagCheckResult.asEvent(event);
        MinecraftForge.EVENT_BUS.post(forgeEvent);
        return ForgeFlagCheckResult.asNonEvent(forgeEvent);
    }

    @Override
    public boolean post(RegionEvent event) {
        if (event instanceof RegionEvent.Create) {
            ForgeRegionEvent.Create create = new ForgeRegionEvent.Create((RegionEvent.Create) event);
            return MinecraftForge.EVENT_BUS.post(create);
        }
        if (event instanceof RegionEvent.Rename) {
            ForgeRegionEvent.Rename rename = new ForgeRegionEvent.Rename((RegionEvent.Rename) event);
            return MinecraftForge.EVENT_BUS.post(rename);
        }
        if (event instanceof RegionEvent.Remove) {
            ForgeRegionEvent.Remove remove = new ForgeRegionEvent.Remove((RegionEvent.Remove) event);
            return MinecraftForge.EVENT_BUS.post(remove);
        }
        if (event instanceof RegionEvent.UpdateArea) {
            ForgeRegionEvent.UpdateArea updateArea = new ForgeRegionEvent.UpdateArea((RegionEvent.UpdateArea) event);
            return MinecraftForge.EVENT_BUS.post(updateArea);
        }
        return false;
    }

    @Override
    public RegionEvent.UpdateArea post(RegionEvent.UpdateArea event) {
        ForgeRegionEvent.UpdateArea forgeEvent = ForgeRegionEvent.UpdateArea.asEvent(event);
        MinecraftForge.EVENT_BUS.post(forgeEvent);
        return ForgeRegionEvent.UpdateArea.asNonEvent(forgeEvent);
    }


    @Override
    public void post(FlagEvent event) {
        if (event instanceof FlagEvent.AddFlagEvent) {
            ForgeFlagEvent.AddFlagEvent addFlagEvent = new ForgeFlagEvent.AddFlagEvent((FlagEvent.AddFlagEvent) event);
            MinecraftForge.EVENT_BUS.post(addFlagEvent);
        }
        if (event instanceof FlagEvent.RemoveFlagEvent) {
            ForgeFlagEvent.RemoveFlagEvent removeFlagEvent = new ForgeFlagEvent.RemoveFlagEvent((FlagEvent.RemoveFlagEvent) event);
            MinecraftForge.EVENT_BUS.post(removeFlagEvent);
        }
        if (event instanceof FlagEvent.UpdateFlagMessageEvent) {
            ForgeFlagEvent.UpdateFlagMessageEvent updateFlagMessageEvent = new ForgeFlagEvent.UpdateFlagMessageEvent((FlagEvent.UpdateFlagMessageEvent) event);
            MinecraftForge.EVENT_BUS.post(updateFlagMessageEvent);
        }
    }

    @Override
    public FlagEvent.UpdateFlagMessageEvent post(FlagEvent.UpdateFlagMessageEvent event) {
        ForgeFlagEvent.UpdateFlagMessageEvent forgeEvent = ForgeFlagEvent.UpdateFlagMessageEvent.asEvent(event);
        MinecraftForge.EVENT_BUS.post(forgeEvent);
        return ForgeFlagEvent.UpdateFlagMessageEvent.asNonEvent(forgeEvent);
    }
}
