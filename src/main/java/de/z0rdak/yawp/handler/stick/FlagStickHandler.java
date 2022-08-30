package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.core.stick.FlagStick;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

import static de.z0rdak.yawp.util.StickUtil.STICK;

public class FlagStickHandler {

    public static void onCycleFlagStick(ItemStack flagStickItem) {
        CompoundTag nbt = flagStickItem.getTag();
        FlagStick flagStick = new FlagStick(nbt.getCompound(STICK));
        // cycle mode
        flagStick.cycleMode();
        // update stick name
        flagStickItem.getTag().put(STICK, flagStick.serializeNBT());
        StickUtil.setStickName(flagStickItem, StickType.REGION_STICK);
    }
}
