package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.item.PrimedTnt;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecartContainer;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BasePressurePlateBlock;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SimpleWaterloggedBlock;
import net.minecraft.world.level.block.entity.BaseContainerBlockEntity;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.EnderChestBlockEntity;
import net.minecraft.world.level.block.entity.LecternBlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.ToolActions;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.EntityTeleportEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.*;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    @SubscribeEvent
    public static void onElytraFlying(TickEvent.PlayerTickEvent event) {
        if (isServerSide(event.player) && event.phase == TickEvent.Phase.END) {
            ResourceKey<Level> dim = getEntityDim(event.player);
            if (event.player.isFallFlying()) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.player.blockPosition(), NO_FLIGHT, dim, event.player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.player.stopFallFlying();
                });
            }
        }
    }

    /**
     * Prevents traditional attacks from players which use EntityPlayer.attackTargetEntityWithCurrentItem(Entity).
     */
    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (isServerSide(event)) {
            if (event.getTarget() instanceof Player target) {
                Player attacker = event.getPlayer();
                ResourceKey<Level> dim = getEntityDim(attacker);
                FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_PLAYERS, dim, attacker);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    /**
     * Prevents various entities from been attacked from a player. <br>
     * TODO: Flag for all entities
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            Entity eventEntity = event.getTarget();
            ResourceKey<Level> dim = getEntityDim(event.getPlayer());
            BlockPos entityPos = eventEntity.blockPosition();
            FlagCheckEvent checkEvent = null;

            if (isAnimal(eventEntity)) {
                checkEvent = new FlagCheckEvent(entityPos, MELEE_ANIMALS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (isMonster(eventEntity)) {
                checkEvent = new FlagCheckEvent(entityPos, MELEE_MONSTERS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getTarget() instanceof Villager) {
                checkEvent = new FlagCheckEvent(entityPos, MELEE_VILLAGERS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getTarget() instanceof WanderingTrader) {
                checkEvent = new FlagCheckEvent(entityPos, MELEE_WANDERING_TRADER, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (isServerSide(event)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), ITEM_PICKUP, getEntityDim(event.getPlayer()), event.getPlayer());
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        Player player = event.getCausedByPlayer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getParentB().blockPosition(), ANIMAL_BREEDING, getEntityDim(player), event.getCausedByPlayer());
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onAnimalTameAttempt(AnimalTameEvent event) {
        Player player = event.getTamer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getAnimal().blockPosition(), ANIMAL_TAMING, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPlayer().blockPosition(), LEVEL_FREEZE, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPlayer().blockPosition(), XP_FREEZE, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                event.setAmount(0);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPlayer().blockPosition(), XP_PICKUP, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                event.getOrb().remove(Entity.RemovalReason.DISCARDED);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPvpAction(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof Player && dmgSourceEntity instanceof Player) {
                Player playerTarget = (Player) hurtEntity;
                Player playerSource = (Player) dmgSourceEntity;
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), NO_PVP, getEntityDim(playerSource), playerSource);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerHurt(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof Player) {
                Player playerTarget = (Player) hurtEntity;
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), INVINCIBLE, getEntityDim(playerTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }


    /* TODO: Is this test even necessary anymore?
     *   - There is already a PVP flag for onHurt in place
     * */
    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            if (dmgSourceEntity instanceof Player && event.getEntityLiving() instanceof Player) {
                Player dmgTarget = (Player) event.getEntityLiving();
                Player dmgSource = ((Player) dmgSourceEntity);
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), MELEE_PLAYERS, getEntityDim(dmgSource), dmgSource);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                // another check for PVP - this does not prevent knock-back? but prevents dmg
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerKnockback(LivingKnockBackEvent event) {
        if (isServerSide(event)) {
            if (event.getEntityLiving() instanceof Player) {
                Player dmgTarget = (Player) event.getEntityLiving();
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), KNOCKBACK_PLAYERS, getEntityDim(dmgTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    sendFlagMsg(onDeny);
                });
                checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), INVINCIBLE, getEntityDim(dmgTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), BREAK_BLOCKS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                updateBlockState((Level) event.getWorld(), event.getPos());
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() != null && event.getEntity() instanceof Player player) {

                // TODO: Needs to be checked at right click use?
                Set<String> entities = FlagConfig.getBreakFlagEntities();
                boolean isBlockCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                    ResourceLocation blockName = event.getPlacedBlock().getBlock().getRegistryName();
                    return blockName != null && blockName.equals(entityResourceLocation);
                });
                if (isBlockCovered) {
                    // TODO:
                }

                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), PLACE_BLOCKS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    updateBlockState((Level) event.getWorld(), event.getPos());
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (isServerSide(event)) {
            Entity target = event.getTarget();
            Player player = event.getPlayer();
            // FIXME: Tags not considered yet
            Set<String> entityTags = FlagConfig.getBreakFlagEntityTags();
            Set<String> entities = FlagConfig.getBreakFlagEntities();
            boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                return target.getType().getRegistryName() != null && target.getType().getRegistryName().equals(entityResourceLocation);
            });
            if (isBlockEntityCovered) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), BREAK_BLOCKS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    public static void updateBlockState(Level world, BlockPos pos) {
        world.updateNeighborsAt(pos, world.getBlockState(pos).getBlock());
    }

    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (!event.getWorld().isClientSide) {
            Explosion explosion = event.getExplosion();
            // Extra check for player initiated explosion here. Should normally be prevented by not allowing
            // the ignition to begin with.
            if (event.getExplosion().getExploder() instanceof PrimedTnt tntEntity) {
                if (tntEntity.getOwner() instanceof Player player) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos(explosion.getPosition()), IGNITE_EXPLOSIVES, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
            }
            if (explosion.getSourceMob() instanceof Monster monster) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos(explosion.getPosition()), MOB_GRIEFING, getEntityDim(monster), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (isServerSide(event)) {
            Player player = (Player) event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_BONEMEAL, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        if (isServerSide(event)) {
            // handle player teleportation using ender pearls
            if (event instanceof EntityTeleportEvent.EnderPearl enderPearlEvent) {
                Player player = enderPearlEvent.getPlayer();
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos(event.getTarget()), USE_ENDERPEARL_TO_REGION, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                if (flagState == FlagState.DENIED)
                    return;

                checkEvent = new FlagCheckEvent(new BlockPos(event.getTarget()), USE_ENDERPEARL_FROM_REGION, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                /* FIXME: refund pearl - duplication bug with e.g. origins mod
                int count = player.getHeldItem(player.getActiveHand()).getCount();
                player.getHeldItem(player.getActiveHand()).setCount(count + 1);
                player.inventory.setChanged();
                return;
                */
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolModificationEvent event) {
        if (!event.getWorld().isClientSide()) {
            Player player = event.getPlayer();
            BlockPos target = event.getPos();
            ResourceKey<Level> dim = getEntityDim(player);
            FlagCheckEvent checkEvent = new FlagCheckEvent(target, TOOL_SECONDARY_USE, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
            if (flagState == FlagState.DENIED)
                return;

            if (event.getToolAction().equals(ToolActions.AXE_STRIP)) {
                checkEvent = new FlagCheckEvent(target, AXE_STRIP, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolAction().equals(ToolActions.HOE_TILL)) {
                checkEvent = new FlagCheckEvent(target, HOE_TILL, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolAction().equals(ToolActions.SHOVEL_FLATTEN)) {
                checkEvent = new FlagCheckEvent(target, SHOVEL_PATH, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            BlockEntity targetEntity = event.getWorld().getBlockEntity(event.getPos());
            ResourceKey<Level> dim = getEntityDim(player);
            boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

            // used to allow player to place blocks when shift clicking container or usable bock
            boolean hasEmptyHands = hasEmptyHands(player);
            BlockHitResult pos = event.getHitVec();
            boolean isBlock = pos != null && pos.getType() == HitResult.Type.BLOCK;

            if (player.isShiftKeyDown() && hasEmptyHands || !player.isShiftKeyDown()) {
                if (isBlock) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(pos.getBlockPos(), USE_BLOCKS, dim, player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                        event.getWorld().updateNeighborsAt(pos.getBlockPos(), event.getWorld().getBlockState(pos.getBlockPos()).getBlock());
                    });
                }
                // Note: following flags are already covered with use_blocks
                // check for ender chest access
                if (isEnderChest) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), ENDER_CHEST_ACCESS, dim, player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
                // check for container access
                if (isContainer) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), CONTAINER_ACCESS, dim, player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
            }
            if (!hasEmptyHands) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }


    @SubscribeEvent
    public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            boolean isMinecartContainer = event.getTarget() instanceof AbstractMinecartContainer;
            if (isMinecartContainer) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), CONTAINER_ACCESS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteractSpecific event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
            if (!hasEmptyHands(player)) {

                checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    private static boolean hasEmptyHands(Player player) {
        return player.getItemInHand(InteractionHand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getItemInHand(InteractionHand.OFF_HAND).getItem().equals(Items.AIR);
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
            if (!hasEmptyHands(player)) {
                checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.RightClickItem event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent;

                if (!hasEmptyHands(player)) {
                    checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
            checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });


        }
    }

    /**
     * Prevents players from using activator blocks like pressure plates
     * TODO: This is very jank implementation. Needs to be tested with multiple players.
     * TODO: Move check to activator block itself
     */
    @SubscribeEvent
    public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
        if (isServerSide(event)) {
            Block block = event.getWorld().getBlockState(event.getPos()).getBlock();
            BlockPos pos = event.getPos();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (block instanceof BasePressurePlateBlock) {
                AABB areaAbovePressurePlate = new AABB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
                List<Player> players = event.getWorld().getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
                final FlagState[] cumulativeState = {FlagState.UNDEFINED};
                Map<Player, FlagCheckEvent> playerCheckEventMap = new HashMap<>();
                for (Player player : players) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_BLOCKS, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    playerCheckEventMap.put(player, checkEvent);
                }
                for (Map.Entry<Player, FlagCheckEvent> entry : playerCheckEventMap.entrySet()) {
                    FlagState state = HandlerUtil.processCheck(entry.getValue(), null, HandlerUtil::sendFlagMsg);
                    if (state == FlagState.DENIED) {
                        cumulativeState[0] = state;
                    }
                }
                if (cumulativeState[0] == FlagState.DENIED) {
                    event.setCanceled(true);
                }
            }
        }
    }

    /**
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     * TODO: Check event.getFilledBucket again to maybe simplify this check
     */
    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (event.getTarget() != null) {
                HitResult pos = event.getTarget();
                BlockPos targetPos = new BlockPos(event.getTarget().getLocation());
                // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
                int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();
                // placing fluid
                if (bucketItemMaxStackCount == 1) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, PLACE_FLUIDS, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
                // scooping fluid (breaking fluid)
                if (bucketItemMaxStackCount > 1) {
                    boolean isWaterlogged = false;
                    boolean isFluid = false;
                    if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
                        BlockState blockState = event.getWorld().getBlockState(targetPos);
                        // check for waterlogged block
                        if (blockState.getBlock() instanceof SimpleWaterloggedBlock) {
                            isWaterlogged = blockState.getValue(BlockStateProperties.WATERLOGGED);
                        }
                        // check if entityPos has a fluid tag
                        if (ForgeRegistries.FLUIDS.tags() != null) {
                            isFluid = ForgeRegistries.FLUIDS.tags().getTagNames().anyMatch(tag -> blockState.getFluidState().is(tag));
                        }
                        if (isWaterlogged || isFluid) {
                            FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, SCOOP_FLUIDS, getEntityDim(player), player);
                            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                                return;
                            }
                            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                                event.setCanceled(true);
                                sendFlagMsg(onDeny);
                            });
                        }
                    }
                }
            }
        }
    }

    /**
     * TODO: Flag for team chat
     * Note: message received from server but not distributed to all clients
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() != null) {
            ServerPlayer player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), SEND_MESSAGE, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            Player player = event.getParseResults().getContext().getSource().getPlayerOrException();
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), EXECUTE_COMMAND, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        } catch (CommandSyntaxException e) {
            // Most likely thrown because command was not send by a player.
            // This is fine because we don't want this flag to be triggered from non-players entities
        }
    }

    @SubscribeEvent
    public static void onPlayerAttemptSleep(SleepingTimeCheckEvent event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            event.getSleepingLocation().ifPresent((pos) -> {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SLEEP, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setResult(Event.Result.DENY);
                    sendFlagMsg(onDeny);
                });
            });
        }
    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (isServerSide(event)) {
            BlockPos newSpawn = event.getNewSpawn();
            Player player = event.getPlayer();
            if (newSpawn != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(newSpawn, SET_SPAWN, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }

    }

    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntityItem().blockPosition(), ITEM_DROP, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                player.addItem(event.getEntityItem().getItem());
                player.getInventory().setChanged();
                sendFlagMsg(onDeny);
            });
        }
    }

    /**
     * Idea: Flags for different animals to mount
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (isServerSide(event)) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            if (event.getEntityMounting() instanceof Player) {
                Player player = (Player) event.getEntityMounting();
                FlagCheckEvent checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_MOUNTING, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                if (event.isDismounting()) {
                    /* FIXME: Disabled in 1.16.5. Canceling event breaks unmounting. Wait for 1.17 fix: https://bugs.mojang.com/browse/MC-202202
                    checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_UNMOUNTING, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                    */
                }
            }
        }
    }
}