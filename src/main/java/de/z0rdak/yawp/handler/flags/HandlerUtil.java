package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.*;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.WaterAnimal;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.monster.Shulker;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.FlagMessage.REGION_TEMPLATE;

public final class HandlerUtil {

    /**
     * Processes the given flag check event and executes the given consumers if the flag is allowed or denied. <br>
     * The flag check event is evaluated and posted to the event bus. <br>
     * If the flag is allowed, the onAllow consumer is executed, otherwise onDeny. <br>
     *
     * @param checkEvent the flag check event to process
     * @param onAllow    the consumer to execute if the flag is allowed
     * @param onDeny     the consumer to execute if the flag is denied
     * @return the flag state of the result
     */
    public static FlagState processCheck(FlagCheckEvent checkEvent, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckResult result = evaluate(checkEvent);
        MinecraftForge.EVENT_BUS.post(result);
        if (result.getFlagState() == FlagState.ALLOWED
                || result.getFlagState() == FlagState.UNDEFINED
                || result.getFlagState() == FlagState.DISABLED) {
            if (onAllow != null)
                onAllow.accept(result);
        }
        if (result.getFlagState() == FlagState.DENIED) {
            if (onDeny != null)
                onDeny.accept(result);
        }
        return result.getFlagState();
    }

    private HandlerUtil() {
    }

    public static ResourceKey<Level> getEntityDim(Entity entity) {
        return entity.getCommandSenderWorld().dimension();
    }

    public static boolean isAnimal(Entity entity) {
        return entity instanceof Animal || entity instanceof WaterAnimal;
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
        return entity instanceof AbstractVillager;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof Player;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof Monster
                || entity instanceof Slime
                || entity instanceof FlyingMob
                || entity instanceof EnderDragon
                || entity instanceof Shulker;
    }

    /**
     * Sends the flag message for the given flag check event. <br>     *
     *
     * @param result the flag check event to send the message for
     */
    public static void sendFlagMsg(FlagCheckResult result) {
        IProtectedRegion responsibleRegion = result.getResponsible();
        IFlag flag = responsibleRegion.getFlag(result.getRegionFlag().name);
        if (flag == null || result.getFlagState() == FlagState.UNDEFINED || result.getFlagState() == FlagState.DISABLED) {
            return;
        }
        boolean isFlagMuted = flag.getFlagMsg().isMuted() || responsibleRegion.isMuted();
        // If not muted and the event is a player event, send the message
        if (!isFlagMuted && result.getPlayer() != null && result.getPlayer() instanceof PlayerEntity) {
            Map<String, String> msgSubstitutes = FlagMessage.defaultSubstitutesFor(result);
            msgSubstitutes.put(REGION_TEMPLATE, responsibleRegion.getName());
            MutableComponent flagMsg = FlagMessage.buildFrom(result, msgSubstitutes);
            MessageUtil.sendNotification(result.getPlayer(), flagMsg);
        }
    }

    /**
     * Evaluates the given flag check event and returns the result. <br>
     *
     * @param checkEvent the flag check event to evaluate
     * @return the result of the flag check event
     */
    public static FlagCheckResult evaluate(FlagCheckEvent checkEvent) {
        BlockPos target = checkEvent.getTarget();
        Player player = checkEvent.getPlayer();
        RegionFlag regionFlag = checkEvent.getRegionFlag();
        IProtectedRegion responsibleRegion = getResponsible(target, checkEvent.getDimension());
        FlagContainer flags = getFlagsRecursive(responsibleRegion, null);
        FlagState flagState = flags.flagState(regionFlag.name);
        if (flagState == FlagState.UNDEFINED) {
            return new FlagCheckResult(regionFlag, FlagState.UNDEFINED, target, responsibleRegion, player);
        } else {
            // now we know that there is a flag active (either allowed or disallowed,
            // but we need to check the correct region which is responsible,
            // because a parent could override the child
            FlagCorrelation flagCorrelation = getFlagCorrelation(responsibleRegion, regionFlag, new FlagCorrelation(responsibleRegion, null));
            FlagState state = getFlagState(flagCorrelation.region, regionFlag, player);
            return new FlagCheckResult(regionFlag, state, target, flagCorrelation.region, player);
        }
    }

    /**
     * Gets the responsible region for the given position and dimension. <br>
     * The responsible region is the region with the highest priority among all involved regions at the given location and dimension. <br>
     * If no Local Region is defined at the given position, the dimensional region is responsible and returned. <br>
     *
     * @param pos the position to get the responsible region for
     * @param dim the dimension to get the responsible region for
     * @return the responsible region for the given position and dimension
     */
    private static IProtectedRegion getResponsible(BlockPos pos, ResourceKey<Level> dim) {
        IMarkableRegion region = getInvolvedRegionFor(pos, dim);
        if (region == null) {
            return RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
        }
        return region;
    }

    /**
     * Gets all active regions which contain the provided position in the given dimension. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return all active regions which contain the given location and dimension
     */
    private static List<IMarkableRegion> getInvolvedRegionsFor(BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    /**
     * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    @Nullable
    private static IMarkableRegion getInvolvedRegionFor(BlockPos position, ResourceKey<Level> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    public static FlagState getFlagState(BlockPos pos, ResourceKey<Level> dim, RegionFlag flag, @Nullable Player player) {
        IProtectedRegion region = getResponsible(pos, dim);
        return getFlagState(region, flag, player);
    }

    /**
     * Gets the flag state for the given region and flag. <br>
     * If the player is null, the flag state is returned as is. <br>
     * If the player is not null, the flag state is checked against the player's permissions. <br>
     * If the player is permitted, the flag state is returned as allowed. <br>
     *
     * @param region the region to get the flag state for
     * @param flag   the flag to get the state for
     * @param player the player to check the flag state against
     * @return the flag state for the given region and flag
     */
    private static FlagState getFlagState(IProtectedRegion region, RegionFlag flag, @Nullable Player player) {
        if (player == null) {
            return region.getFlagContainer().flagState(flag.name);
        } else {
            boolean isPermitted = region.permits(player);
            if (isPermitted) {
                return FlagState.ALLOWED;
            } else {
                return region.getFlagContainer().flagState(flag.name);
            }
        }
    }

    public static Map<String, FlagCorrelation> getFlagMapRecursive(IProtectedRegion region, Map<String, FlagCorrelation> carry) {
        if (carry == null) {
            carry = region.getFlags().stream()
                    .collect(Collectors.toMap(IFlag::getName, flag -> new FlagCorrelation(region, flag), (a, b) -> b, HashMap::new));
        }
        if (region.equals(region.getParent())) { // global region has itself as parent
            return carry;
        }
        Set<Map.Entry<String, IFlag>> parentFlags = region.getParent().getFlagContainer().entrySet();
        for (Map.Entry<String, IFlag> entry : parentFlags) {
            String flagName = entry.getKey();
            IFlag flag = entry.getValue();
            boolean parentFlagOverrides = flag.doesOverride();
            boolean existingFlag = carry.containsKey(flagName);
            if (parentFlagOverrides && existingFlag) {
                carry.remove(flagName);
                carry.put(flagName, new FlagCorrelation(region.getParent(), flag));
            }
            if (!existingFlag) {
                carry.put(flagName, new FlagCorrelation(region, flag));
            }
        }
        return getFlagMapRecursive(region.getParent(), carry);
    }



    /**
     * Recursively gets all flags defined in the region hierarchy of the provided region, including all it's parents. <br></br>
     * This is used, when we have determined the responsible region for an Event or a BlockPos. <br></br>
     * We need to check all flags of the responsible region and all its parents to determine the final flag state. <br></br>
     *
     * @param region the region (and its parents) to get active flags
     * @param carry  a flag container, holding information about region flags
     * @return a flag container of all active flags of the given region including its parents.
     */
    public static FlagContainer getFlagsRecursive(IProtectedRegion region, FlagContainer carry) {
        if (region.equals(region.getParent())) { // global region has itself as parent
            return carry == null ? region.getFlagContainer().deepCopy() : carry;
        }
        if (carry == null) { // effectively make a deep copy of the flag container
            carry = region.getFlagContainer().deepCopy();
        }
        Map<String, IFlag> activeParentFlags = region.getParent().getFlagContainer().getActiveFlags();
        for (Map.Entry<String, IFlag> entry : activeParentFlags.entrySet()) {
            String flagName = entry.getKey();
            IFlag flag = entry.getValue();
            boolean parentFlagOverrides = flag.doesOverride();
            boolean existingFlag = carry.contains(flagName);
            if (parentFlagOverrides && existingFlag) {
                carry.remove(flagName);
                carry.put(flagName, flag);
            }
            if (!existingFlag) {
                carry.put(flagName, flag);
            }
        }
        return getFlagsRecursive(region.getParent(), carry);
    }

    /**
     * Recursively gets the region hierarchy for the given region. <br>
     * The region hierarchy is a list of regions starting with the given region and ending with the global region. <br>
     *
     * @param region the region to get the hierarchy for
     * @param carry  the list to carry the hierarchy (initially empty)
     * @return the region hierarchy for the given region
     */
    public static List<IProtectedRegion> getRegionHierarchy(IProtectedRegion region, List<IProtectedRegion> carry) {
        if (region.equals(region.getParent())) { // global region has itself as parent
            return carry.isEmpty() ? Collections.singletonList(region) : carry;
        }
        carry.add(region);
        return getRegionHierarchy(region.getParent(), carry);
    }

    /**
     * Recursively gets the flag correlation for the given region and flag. <br>
     * The flag correlation is the region and flag pair which is responsible for the flag state. <br>
     * The flag correlation is determined by checking the flag state of the region and its parents. <br>
     * If the flag is allowed or denied, the flag correlation is set. <br>
     * If the flag is undefined, the parent region is checked. <br>
     * If the parent region is the global region, the flag correlation is returned. <br>
     *
     * @param region     the region to get the flag correlation for
     * @param regionFlag the region flag to get the correlation for
     * @param carry      the flag correlation to carry (initially null)
     * @return the flag correlation for the given region and flag
     */
    private static FlagCorrelation getFlagCorrelation(IProtectedRegion region, RegionFlag regionFlag, @Nullable FlagCorrelation carry) {
        if (region.equals(region.getParent())) {
            if (region.getFlagContainer().flagState(regionFlag.name) != FlagState.UNDEFINED) {
                IFlag flag = region.getFlag(regionFlag.name);
                if (flag.doesOverride()) {
                    carry = new FlagCorrelation(region, flag);
                }
            }
            return carry;
        }
        FlagState flagState = region.getFlagContainer().flagState(regionFlag.name);
        if (flagState != FlagState.UNDEFINED) {
            // allowed or denied
            carry = new FlagCorrelation(region, region.getFlag(regionFlag.name));
        }
        return getFlagCorrelation(region.getParent(), regionFlag, carry);
    }

    public static FlagCorrelation getResponsibleFlag(IProtectedRegion region, RegionFlag regionFlag, @Nullable FlagCorrelation carry) {
        if (carry == null) {
            if (region.getFlagContainer().get(regionFlag.name).isActive()) {
                IFlag flag = region.getFlag(regionFlag.name);
                carry = new FlagCorrelation(region, flag);
            } else
                carry = new FlagCorrelation(region, null);
        }
        if (region.equals(region.getParent())) {
            IFlag flag = region.getFlag(regionFlag.name);
            if (flag.doesOverride()) {
                carry = new FlagCorrelation(region, flag);
            }
            return carry;
        }
        return getResponsibleFlag(region.getParent(), regionFlag, carry);
    }
}
