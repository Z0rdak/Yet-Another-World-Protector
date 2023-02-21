package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.Objects;

import static de.z0rdak.yawp.util.StickUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class StickInteractionHandler {

    private StickInteractionHandler() {
    }

    @SubscribeEvent
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getLevel().isClientSide) {
            Player player = event.getEntity();
            // TODO: Maybe check if player is allowed to mark block
            ItemStack involvedItemStack = event.getItemStack();
            if (!involvedItemStack.equals(ItemStack.EMPTY) && isVanillaStick(involvedItemStack)) {
                StickType stickType = getStickType(involvedItemStack);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    MarkerStickHandler.onMarkBlock(involvedItemStack, event);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onCycleMode(PlayerInteractEvent.RightClickItem event) {
        if (!event.getLevel().isClientSide) {
            ItemStack involvedItemStack = event.getItemStack();
            // is some valid mod stick
            if (!involvedItemStack.equals(ItemStack.EMPTY)
                    && hasNonNullTag(involvedItemStack)
                    && involvedItemStack.getTag().contains(STICK)) {
                HitResult blockLookingAt = event.getEntity().pick(20.0d, 0.0f, false);
                boolean targetIsAir;
                if (blockLookingAt.getType() == HitResult.Type.BLOCK) {
                    BlockPos blockpos = ((BlockHitResult) blockLookingAt).getBlockPos();
                    BlockState blockstate = event.getLevel().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = blockLookingAt.getType() == HitResult.Type.MISS;
                }

                if (event.getEntity().isShiftKeyDown() && targetIsAir) {
                    StickType stickType = getStickType(involvedItemStack);
                    if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                        // FIXME: cycling mode is disabled for now because there is only one working area type
                        //MarkerStickHandler.onCycleRegionMarker(involvedItemStack);
                    }
                }
            }
        }
    }

    /**
     * Handles action when renaming mod sticks in an anvil.
     * This is used to create a mod stick or to define a region by renaming a valid RegionMarker stick.
     */
    @SubscribeEvent
    public static void onStickRename(AnvilRepairEvent event) {
        Player player = event.getEntity();
        if (!player.getCommandSenderWorld().isClientSide) {
            ItemStack outputItem = event.getOutput();
            ItemStack inputItem = event.getLeft();
            ItemStack ingredientInput = event.getRight();
            boolean hasStickTag = outputItem.hasTag() && outputItem.getTag() != null && outputItem.getTag().contains(STICK);
            // TODO: Check if player is allowed to create region -
            // FIXME: this is not possible when player has no rights for dimension - a parent would need to be selected
            // stick set region parent where player == owner
            if (hasStickTag) {
                MarkerStickHandler.onCreateRegion(event);
            }
            boolean isInputAndOutputStick = ItemStack.isSame(outputItem, Items.STICK.getDefaultInstance())
                    && ItemStack.isSame(inputItem, Items.STICK.getDefaultInstance());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                onCreateStick(event);
            }
        }
    }

    /**
     * Edits the NBT data of the renamed stick to "transform" it to the corresponding mod stick.
     * @param event the event data from renaming the stick item
     */
    private static void onCreateStick(AnvilRepairEvent event) {
        Player player = event.getEntity();
        ItemStack outputItem = event.getOutput();
        ItemStack inputItem = event.getLeft();
        StickType type = StickType.of(outputItem.getHoverName().getString());
        if (type != StickType.UNKNOWN) {
            // split stack and only create one stick, also refund xp
            inputItem.setCount(outputItem.getCount() - 1);
            player.addItem(inputItem);
            event.setBreakChance(0.0f);
            player.giveExperienceLevels(1);
            initMarkerNbt(outputItem, type, event.getEntity().getCommandSenderWorld().dimension());
            player.getInventory().setChanged();
        }
    }
}
