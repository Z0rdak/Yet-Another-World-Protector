package de.z0rdak.regionshield.handler;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.core.area.AreaType;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.stick.MarkerStick;
import de.z0rdak.regionshield.util.RegionUtil;
import de.z0rdak.regionshield.util.StickType;
import de.z0rdak.regionshield.util.StickUtil;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.regionshield.util.StickUtil.*;

@Mod.EventBusSubscriber(modid = RegionShield.MODID)
public class StickInteractionHandler {

    private StickInteractionHandler() {
    }

    @SubscribeEvent
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            ItemStack involvedItemStack = event.getItemStack();
            if (!involvedItemStack.equals(ItemStack.EMPTY) && isStick(involvedItemStack)) {
                StickType stickType = getStickType(involvedItemStack);

                boolean isShiftPressed = player.isShiftKeyDown();
                BlockRayTraceResult blockRayTraceResult = event.getHitVec();
                BlockPos pos = blockRayTraceResult.getBlockPos();
                RayTraceResult.Type traceResultType = blockRayTraceResult.getType();

                switch (stickType) {
                    case MARKER:
                        onMarkBlock(involvedItemStack, event);
                        break;
                    case REGION_STICK:
                        break;
                    case FLAG_STICK:
                        break;
                    default:
                        break;
                }
            }
        }
        // TODO: check block and handle stick action accordingly
        // TODO: rendering and charge use needs to be implemented in stickitem mixin
    }

    public static void onMarkBlock(ItemStack involvedItem, PlayerInteractEvent.RightClickBlock event) {
        MarkerStick marker = new MarkerStick(involvedItem.getTag().getCompound(STICK));
        AreaType areaType = marker.getAreaType();
        if (areaType == AreaType.UNKNOWN) {
            RegionShield.LOGGER.warn("Unknown area type on marking - should really not happening");
            return;
        }
        if (event.getPlayer().isShiftKeyDown()) {
            marker.setTeleportPos(event.getPos());
            involvedItem.getTag().put(STICK, marker.serializeNBT());
            return;
        }
        // add block to NBT list
        marker.addMarkedBlock(event.getPos());
        // check whether marked blocks form a valid marked area
        marker.checkValidArea();
        involvedItem.getTag().put(STICK, marker.serializeNBT());
        setStickName(involvedItem, StickType.MARKER);
    }

    @SubscribeEvent
    public static void onCycleMode(PlayerInteractEvent.RightClickItem event) {
        if (!event.getWorld().isClientSide) {
            ItemStack involvedItemStack = event.getItemStack();
            // is some valid mod stick
            if (!involvedItemStack.equals(ItemStack.EMPTY) && involvedItemStack.hasTag()
                    && involvedItemStack.getTag() != null && involvedItemStack.getTag().contains(STICK)) {

                RayTraceResult blockLookingAt = event.getPlayer().pick(20.0d, 0.0f, false);
                boolean targetIsAir = false;
                if (blockLookingAt.getType() == RayTraceResult.Type.BLOCK) {
                    BlockPos blockpos = ((BlockRayTraceResult) blockLookingAt).getBlockPos();
                    BlockState blockstate = event.getWorld().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                }
                if (blockLookingAt.getType() == RayTraceResult.Type.MISS) {
                    targetIsAir = true;
                }

                if (event.getPlayer().isShiftKeyDown() && targetIsAir) {
                    StickType stickType = getStickType(involvedItemStack);
                    CompoundNBT nbt = involvedItemStack.getTag();
                    switch (stickType) {
                        case REGION_STICK:

                            break;

                        case FLAG_STICK:

                            break;

                        case MARKER:
                            MarkerStick marker = new MarkerStick(nbt.getCompound(STICK));
                            AreaType areaType = marker.getAreaType();
                            AreaType nextType = AreaType.values()[(areaType.ordinal() + 1) % AreaType.values().length];
                            // change area nbt, reset marked blocks, set valid to false
                            marker.cycleArea(nextType == AreaType.UNKNOWN ? AreaType.CUBOID : nextType);
                            // update stick name
                            involvedItemStack.getTag().put(STICK, marker.serializeNBT());
                            StickUtil.setStickName(involvedItemStack, StickType.MARKER);
                            break;
                        case UNKNOWN:
                        default:
                            break;
                    }
                }
            }
        }
    }

    /**
     * Handles creation of mod sticks by renaming them in an anvil
     */
    @SubscribeEvent
    public static void onStickRename(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        if (!player.getCommandSenderWorld().isClientSide) {
            ItemStack outputItem = event.getItemResult();
            ItemStack inputItem = event.getItemInput();
            ItemStack ingredientInput = event.getIngredientInput();
            boolean hasStickTag = outputItem.hasTag() && outputItem.getTag() != null && outputItem.getTag().contains(STICK);
            if (hasStickTag) {
                onCreateRegion(event);
            }
            boolean isInputAndOutputStick = ItemStack.isSame(outputItem, Items.STICK.getDefaultInstance())
                    && ItemStack.isSame(inputItem, Items.STICK.getDefaultInstance());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                onCreateStick(event);
            }
        }
    }

    private static void onCreateStick(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        ItemStack outputItem = event.getItemResult();
        ItemStack inputItem = event.getItemInput();
        StickType type = StickType.of(outputItem.getHoverName().getString());
        if (type != StickType.UNKNOWN) {
            // split stack and only create one stick, also refund xp
            inputItem.setCount(outputItem.getCount() - 1);
            player.addItem(inputItem);
            // TODO: Send network packet to force inventory sync
            player.giveExperienceLevels(1);
            outputItem.setCount(1);
            // init NBT
            initStickTag(outputItem, type, event.getPlayer().getCommandSenderWorld().dimension());
            setStickName(outputItem, type);
            setStickToolTip(outputItem, type);
            applyEnchantmentGlint(outputItem);
        }
    }

    private static void onCreateRegion(AnvilRepairEvent event) {
        ItemStack outputItem = event.getItemResult();
        PlayerEntity player = event.getPlayer();
        CompoundNBT stickNBT = outputItem.getTag().getCompound(STICK);
        StickType type = StickType.of(stickNBT.getString(STICK_TYPE));
        switch (type) {
            case MARKER:
                String regionName = outputItem.getHoverName().getString();
                MarkerStick marker = new MarkerStick(stickNBT);
                if (marker.isValidArea()) {
                    AbstractMarkableRegion region = RegionUtil.regionFrom(marker, regionName);
                    // TODO: save region
                    RegionShield.LOGGER.info(region.getName());
                    marker.resetArea();
                    outputItem.getTag().put(STICK, marker.serializeNBT());
                    StickUtil.setStickName(outputItem, type);
                } else {
                    player.sendMessage(new TranslationTextComponent("Could not create region"), player.getUUID());
                }
                break;
            case FLAG_STICK:
                break;
            case REGION_STICK:
                break;
            case UNKNOWN:
            default:
                break;
        }
    }

}
