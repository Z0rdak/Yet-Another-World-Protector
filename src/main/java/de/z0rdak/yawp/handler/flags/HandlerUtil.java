package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.FlyingEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.merchant.villager.AbstractVillagerEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.WaterMobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class HandlerUtil {

    private HandlerUtil(){}

    public static RegistryKey<World> getEntityDim(Entity entity){
        return entity.getCommandSenderWorld().dimension();
    }

    public static boolean isAnimal(Entity entity){
        return entity instanceof AnimalEntity || entity instanceof WaterMobEntity;
    }

    public static boolean isServerSide(EntityEvent event) {
        return isServerSide(event.getEntity());
    }

    public static boolean isServerSide(BlockEvent event) {
        return !event.getWorld().isClientSide();
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getCommandSenderWorld().isClientSide;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof AbstractVillagerEntity;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof PlayerEntity;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof MonsterEntity
                || entity instanceof SlimeEntity
                || entity instanceof FlyingEntity
                || entity instanceof EnderDragonEntity
                || entity instanceof ShulkerEntity;
    }

    public static boolean handleAndSendMsg(Event event, FlagCheckEvent flagCheck) {
        sendFlagMsg(flagCheck);
        event.setCanceled(flagCheck.isDenied());
        return flagCheck.isDenied();
    }

    public static void sendFlagMsg(FlagCheckEvent flagCheck) {
        boolean isPlayerEvent = flagCheck instanceof PlayerFlagEvent;
        boolean hasLocalRegion = flagCheck.getLocalRegion() != null;
        if (!hasLocalRegion && flagCheck.isDeniedInDim()) {
            if (!flagCheck.getDimRegion().isMuted()) {
                IFlag flag = flagCheck.getDimRegion().getFlag(flagCheck.getRegionFlag().name);
                sendFlagMsg(flagCheck, isPlayerEvent, flag, flagCheck.getDimRegion(), FlagConfig.getRawDimFlagMsg());
            }
        }
        if (hasLocalRegion && flagCheck.isDeniedLocal()) {
            if (!flagCheck.getLocalRegion().isMuted()) {
                IFlag flag = flagCheck.getLocalRegion().getFlag(flagCheck.getRegionFlag().name);
                sendFlagMsg(flagCheck, isPlayerEvent, flag, flagCheck.getLocalRegion(), FlagConfig.getRawLocalFlagMsg());
            }
        }
    }

    private static void sendFlagMsg(FlagCheckEvent flagCheck, boolean isPlayerEvent, IFlag flag, IProtectedRegion region, String defaultFlagConfigMsg) {
        boolean isFlagMuted = flag.getFlagMsg().isMuted();
        if (!isFlagMuted && isPlayerEvent) {
            PlayerFlagEvent playerFlagEvent = (PlayerFlagEvent) flagCheck;
            playerFlagEvent.getMsgSubstitutes().put("{region}", region.getName());
            String flagConfigMsg = FlagConfig.hasDefaultMsgConfig(flagCheck.getRegionFlag())
                    ? FlagConfig.getDefaultFlagMessage(flagCheck.getRegionFlag())
                    : defaultFlagConfigMsg;
            IFormattableTextComponent flagMsg = buildFlagMsg(playerFlagEvent, flag, flagConfigMsg);
            MessageUtil.sendNotification(playerFlagEvent.getPlayer(), flagMsg);
        }
    }

    // TODO: create custom map impl
    public static Map<String, String> defaultSubstituteMap(RegionFlag flag, IProtectedRegion region, BlockPos pos, @Nullable PlayerEntity player) {
        Map<String, String> substituteMap = new HashMap<>();
        substituteMap.put(FLAG_TEMPLATE, flag.name);
        substituteMap.put(POS_TEMPLATE, MessageUtil.shortBlockPos(pos));
        substituteMap.put(REGION_TEMPLATE, region.getName());
        substituteMap.put(DIM_TEMPLATE, region.getDim().location().toString());
        if (player != null && flag.categories.contains(FlagCategory.PLAYER)) {
            substituteMap.put(PLAYER_TEMPLATE, player.getScoreboardName());
        }
        return substituteMap;
    }

    public static IFormattableTextComponent buildFlagMsg(PlayerFlagEvent flagCheckEvent, IFlag flag) {
        String flagMsgTemplate = flag.getFlagMsg().isDefault()
                ? getDefaultFlagMsgTemplate(flag)
                : flag.getFlagMsg().getMsg();
        String defaultFlagMsg = replaceMatches(flagMsgTemplate, flagCheckEvent);
        return new StringTextComponent(defaultFlagMsg);
    }

    private static String replaceMatches(String flag, PlayerFlagEvent flagCheckEvent) {
        String flagMsg = flag;
        for (Map.Entry<String, String> entry : flagCheckEvent.getMsgSubstitutes().entrySet()) {
            flagMsg = flagMsg.replace(entry.getKey(), entry.getValue());
        }
        return flagMsg;
    }


    public static FlagCheckEvent checkEvent(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion, @Nullable PlayerEntity player) {
        IMarkableRegion involvedRegion = LocalRegions.getInvolvedRegionFor(target, dimRegion.getDim());
        FlagCheckEvent flagCheck = new FlagCheckEvent(dimRegion, involvedRegion, regionFlag);
        if (player != null) {
            // TODO: Make this lazy and/or only execute it when msg is to be displayed
            // FIXME: currently the dim name is set here as {region}, and is corrected later
            Map<String, String> subsMap = defaultSubstituteMap(regionFlag, dimRegion, target, player);
            flagCheck = new PlayerFlagEvent(flagCheck, player, subsMap);
        }
        if (involvedRegion == null) {
            flagCheck.setDeniedLocal(false);
        } else {
            boolean isDeniedLocally = checkDeniedInRegion(involvedRegion, regionFlag, player);
            flagCheck.setDeniedLocal(isDeniedLocally);
        }
        boolean isDeniedInDim = checkDeniedInRegion(dimRegion, regionFlag, player);
        flagCheck.setDeniedInDim(isDeniedInDim);
        return getCheckResult(involvedRegion, flagCheck);
    }

    public static FlagCheckEvent checkEvent(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        return checkEvent(target, regionFlag, dimRegion, null);
    }

    private static boolean checkDeniedInRegion(IProtectedRegion region, RegionFlag regionFlag, @Nullable PlayerEntity player) {
        if (region.isActive()) {
            boolean disallowsPlayer = player == null || !region.permits(player);
            if (region.containsFlag(regionFlag) && disallowsPlayer) {
                IFlag flag = region.getFlag(regionFlag.name);
                // TODO: Check state with allowed
                return flag.isActive();
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    private static FlagCheckEvent getCheckResult(IMarkableRegion involvedRegion, FlagCheckEvent flagCheck) {
        if (flagCheck.getLocalRegion() == null) {
            flagCheck.setDenied(flagCheck.isDeniedInDim());
        } else {
            boolean isDenied = flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal()
                    || !flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal()
                    || flagCheck.isDeniedInDim() && involvedRegion == null;
            flagCheck.setDenied(isDenied);
        }
        return flagCheck;
    }
}
