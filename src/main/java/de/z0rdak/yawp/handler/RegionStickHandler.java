package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.core.stick.RegionStick;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.util.StickUtil.STICK;

public class RegionStickHandler {

    public static void onCycleRegionStick(ItemStack regionStickItem) {
        CompoundNBT nbt = regionStickItem.getTag();
        RegionStick regionStick = new RegionStick(nbt.getCompound(STICK));
        // cycle mode
        regionStick.cycleMode();
        // update stick name
        regionStickItem.getTag().put(STICK, regionStick.serializeNBT());
        StickUtil.setStickName(regionStickItem, StickType.REGION_STICK);
    }
}
